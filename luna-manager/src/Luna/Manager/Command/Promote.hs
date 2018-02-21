{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Command.Promote where

import Prologue hiding (FilePath, (<.>))

import           Control.Exception.Safe            as Exception
import           Control.Monad.State.Layered
import qualified Crypto.Hash                  as Crypto
import qualified Data.Text                         as Text
import           Filesystem.Path.CurrentOS         (FilePath, parent, encodeString, fromText, (</>), (<.>), filename)

import qualified Luna.Manager.Archive              as Archive
import           Luna.Manager.Command.NextVersion  (PromotionInfo(..), TargetVersionType(..), VersionUpgradeException(..), createNextVersion, newVersion, oldVersion, appName)
import           Luna.Manager.Command.Options      (Options, NextVersionOpts, PromoteOpts)
import qualified Luna.Manager.Command.Options      as Opts
import qualified Luna.Manager.Logger               as Logger
import           Luna.Manager.Network              (MonadNetwork, downloadWithProgressBarTo)
import           Luna.Manager.Component.Pretty     (showPretty)
import           Luna.Manager.Component.Repository (RepoConfig)
import           Luna.Manager.Component.Version    (Version)
import qualified Luna.Manager.Component.Repository as Repository
import qualified Luna.Manager.Shell.Shelly         as Shelly

import           Luna.Manager.System               (makeExecutable, generateChecksum)
import           Luna.Manager.System.Env
import           Luna.Manager.System.Host          (currentHost, System(..))
import           Luna.Manager.System.Path          (expand)

default (Text.Text)

type MonadPromote m = (MonadGetter Options m, MonadStates '[EnvConfig, RepoConfig] m, MonadNetwork m, Shelly.MonadSh m, Shelly.MonadShControl m, MonadIO m)


-- return the new name of the package, based on the full path to the old one
-- note that this drops the extension!
-- if called with ~/some/path/luna-studio
newPackageName :: FilePath -> Version -> Text
newPackageName pkgPath version = if length chunks < 3
    then fName <> "1" -- if the version doesn't contain enough parts, it must be the old one
    else chunks & ix lastIdx .~ prettyV & Text.intercalate "-"  -- replace the last element with the new version
    where fName   = Shelly.toTextIgnore $ filename pkgPath
          chunks  = Text.splitOn "-" fName
          lastIdx = length chunks - 1
          prettyV = showPretty version


renameVersion :: MonadPromote m => FilePath -> FilePath -> Version -> Version -> m ()
renameVersion path repoPath versionOld versionNew = do
    let prettyVersion = showPretty versionNew
        versionFile   = encodeString $ path </> "config" </> "version.txt"
        promoteScript = repoPath </> "scripts_build" </> "promote.py"

    Logger.log $ "Writing new version number: " <> prettyVersion
    liftIO $ writeFile versionFile (convert prettyVersion)
    let argsList = [encodeString path, Text.unpack $ showPretty versionOld, Text.unpack prettyVersion]
    case currentHost of
      Windows -> Shelly.cmd "py" promoteScript argsList
      _       -> Shelly.cmd promoteScript argsList

promote' :: MonadPromote m => FilePath -> FilePath -> Text -> Version -> Version -> m ()
promote' pkgPath repoPath name versionOld versionNew = do
    Logger.log "Unpacking the package"
    extracted <- Archive.unpack 1.0 "unpacking_progress" pkgPath

    renameVersion extracted repoPath versionOld versionNew

    let correctPath = (parent extracted) </> (convert name)
    Logger.log $ "Renaming " <> (Shelly.toTextIgnore extracted) <> " to " <> (Shelly.toTextIgnore correctPath)
    Shelly.mv extracted correctPath `Exception.catchAny` (\(e :: SomeException) ->
        Logger.warning $ "Failed to rename the extracted folder.\n" <> (convert $ displayException e))

    Logger.log $ "Compressing the package"
    let newName = newPackageName pkgPath versionNew
    compressed <- Archive.pack correctPath newName
    generateChecksum  @Crypto.SHA256 $ (parent correctPath) </> Shelly.fromText (newName <> ".tar.gz")

    Logger.log "Cleaning up"
    Shelly.rm_rf correctPath `Exception.catchAny` (\(e :: SomeException) ->
        Logger.warning $ "Failed to clean up after extracting.\n" <> (convert $ displayException e))


promoteLinux :: MonadPromote m => FilePath -> FilePath -> Text -> Version -> Version -> m ()
promoteLinux pkgPath repoPath name versionOld versionNew = do
    Logger.log "Ensuring the AppImage is executable"
    makeExecutable pkgPath

    Logger.log "Unpacking AppImage"
    Shelly.silently $ Shelly.cmd pkgPath "--appimage-extract"

    let appDir = "squashfs-root" :: FilePath
    renameVersion (appDir </> "usr") repoPath versionOld versionNew

    Logger.log "Downloading appImageTool"
    let aitUrl  = "https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage"
        baseDir = parent pkgPath
    appImageTool <- downloadWithProgressBarTo aitUrl "."
    makeExecutable appImageTool

    Logger.log "Repacking AppImage"
    let aiName    = Shelly.toTextIgnore $ filename pkgPath
        aiNewName = newPackageName pkgPath versionNew <> ".AppImage"
    Shelly.cmd appImageTool appDir aiNewName

    Logger.log "Moving the AppImage"
    Shelly.mv (convert aiNewName) baseDir `Exception.catchAny` (\(e :: SomeException) ->
        Logger.warning $ "Failed to move the AppImage.\n" <> (convert $ displayException e))
        
    Logger.log "Generating checksum"
    generateChecksum  @Crypto.SHA256 $ baseDir </> Shelly.fromText aiNewName

    Logger.log "Cleaning up"
    (Shelly.rm_rf appDir >> Shelly.rm_rf appImageTool) `Exception.catchAny` (\(e :: SomeException) ->
        Logger.warning $ "Failed to clean up after extracting.\n" <> (convert $ displayException e))


promote :: MonadPromote m => FilePath -> FilePath -> PromotionInfo -> m ()
promote pkgPath repoPath prInfo = do
    let name = prInfo ^. appName
        vOld = prInfo ^. oldVersion
    case prInfo ^. newVersion of
        Nothing -> liftIO $ putStrLn "No version to promote"
        Just vNew  -> case currentHost of
            Linux -> promoteLinux pkgPath repoPath name vOld vNew
            _     -> promote'     pkgPath repoPath name vOld vNew


run :: MonadPromote m => PromoteOpts -> m ()
run opts = do
    let cfgPath = convert $ opts ^. Opts.confPath :: FilePath
        pkgPath = convert $ opts ^. Opts.pkgPath  :: FilePath
        verType = if opts ^. Opts.toRelease then Release else Nightly

    pkgFullPath <- expand $ pkgPath
    cfgFullPath <- expand $ cfgPath
    prInfo      <- createNextVersion cfgFullPath verType Nothing
    promote pkgFullPath (parent cfgFullPath) prInfo

    config   <- Repository.parseConfig cfgFullPath
    resolved <- mapM (Repository.resolvePackageApp config) (config ^. Repository.apps)
    repo     <- Repository.getRepo

    let updatedConfig = foldl' Repository.updateConfig config resolved
    Repository.generateConfigYamlWithNewPackage repo updatedConfig $ (parent pkgFullPath) </> "config.yaml"
