{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Luna.Manager.Command.Promote where

import Prologue hiding (FilePath, (<.>))

import           Control.Exception.Safe      as Exception
import           Control.Monad.State.Layered
import qualified Crypto.Hash                 as Crypto
import qualified Data.Text                   as Text
import           Filesystem.Path.CurrentOS   (FilePath, encodeString, filename,
                                              parent, (<.>), (</>))

import qualified Luna.Manager.Archive                   as Archive
import           Luna.Manager.Command.NextVersion       (PromotionInfo (..),
                                                         TargetVersionType (..),
                                                         appName,
                                                         createNextVersion,
                                                         newVersion, oldVersion)
import           Luna.Manager.Command.Options           (Options, PromoteOpts)
import qualified Luna.Manager.Command.Options           as Opts
import           Luna.Manager.Component.PackageConfig
import           Luna.Manager.Component.Pretty          (showPretty)
import           Luna.Manager.Component.Repository      (RepoConfig,
                                                         ResolvedApplication)
import qualified Luna.Manager.Component.Repository      as Repository
import           Luna.Manager.Component.Version         (Version)
import qualified Luna.Manager.Component.WindowsResource as WindowsResource
import qualified Luna.Manager.Logger                    as Logger
import           Luna.Manager.Network                   (MonadNetwork, downloadWithProgressBarTo)
import qualified Luna.Manager.Shell.Shelly              as Shelly

import Luna.Manager.System      (generateChecksum, makeExecutable)
import Luna.Manager.System.Env
import Luna.Manager.System.Host (System (..), currentHost)
import Luna.Manager.System.Path (expand)

default (Text.Text)

type MonadPromote m = (
      MonadGetter Options m
    , MonadStates '[EnvConfig, RepoConfig, PackageConfig] m
    , MonadNetwork m
    , Shelly.MonadSh m
    , Shelly.MonadShControl m
    , MonadIO m
    )


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


renameVersion :: MonadPromote m => FilePath -> FilePath -> Version -> Version -> ResolvedApplication -> m ()
renameVersion path repoPath versionOld versionNew resolvedApplication = do
    let prettyVersion = showPretty versionNew
        versionFile   = encodeString $ path </> "config" </> "version.txt"
        promoteScript = repoPath </> "scripts_build" </> "promote.py"

    Logger.log $ "Writing new version number: " <> prettyVersion
    liftIO $ writeFile versionFile (convert prettyVersion)
    let argsList = [encodeString path, Text.unpack $ showPretty versionOld, Text.unpack prettyVersion]
    case currentHost of
        Windows -> do
            Shelly.cmd "py" promoteScript argsList
            pkgConfig <- get @PackageConfig
            let app        = resolvedApplication ^. Repository.resolvedApp
                appHeader  = app ^. Repository.header
                appName    = appHeader ^. Repository.name
            let mainAppDir = path
                binsFolder = mainAppDir </> (pkgConfig ^. binFolder)
                privateBinsFolder = binsFolder </> (pkgConfig ^. binsPrivate)
                publicBinsFolder  = binsFolder </> (pkgConfig ^. binsPublic)
            let appName' = convert appName
                binary   = publicBinsFolder </> appName' </> appName' <.> "exe"
            Logger.log $ "Updating metadata to: " <> prettyVersion
            WindowsResource.updateWindowsMetadata versionNew privateBinsFolder binary
            Logger.log $ "Signing executables"
            WindowsResource.signWindowsBinaries privateBinsFolder binary
        _       -> Shelly.cmd promoteScript argsList

promote' :: MonadPromote m => FilePath -> FilePath -> Text -> Version -> Version -> ResolvedApplication -> m ()
promote' pkgPath repoPath name versionOld versionNew res = do
    Logger.log "Unpacking the package"
    extracted <- Archive.unpack 1.0 "unpacking_progress" pkgPath Nothing

    renameVersion extracted repoPath versionOld versionNew res

    let correctPath = (parent extracted) </> (convert name)
    Logger.log $ "Renaming " <> (Shelly.toTextIgnore extracted) <> " to " <> (Shelly.toTextIgnore correctPath)
    Shelly.mv extracted correctPath `Exception.catchAny` (\(e :: SomeException) ->
        Logger.warning $ "Failed to rename the extracted folder.\n" <> (convert $ displayException e))

    Logger.log $ "Compressing the package"
    let newName = newPackageName pkgPath versionNew
    compressed <- Archive.pack correctPath newName
    generateChecksum  @Crypto.SHA256 $ (parent correctPath) </> Shelly.fromText (newName <> if currentHost == Windows then ".7z" else ".tar.gz")

    Logger.log "Cleaning up"
    Shelly.rm_rf correctPath `Exception.catchAny` (\(e :: SomeException) ->
        Logger.warning $ "Failed to clean up after extracting.\n" <> (convert $ displayException e))


promoteLinux :: MonadPromote m => FilePath -> FilePath -> Text -> Version -> Version -> ResolvedApplication -> m ()
promoteLinux pkgPath repoPath name versionOld versionNew res = do
    Logger.log "Ensuring the AppImage is executable"
    makeExecutable pkgPath

    Logger.log "Unpacking AppImage"
    Shelly.silently $ Shelly.cmd pkgPath "--appimage-extract"

    let appDir = "squashfs-root" :: FilePath
    renameVersion (appDir </> "usr") repoPath versionOld versionNew res

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


promote :: MonadPromote m => FilePath -> FilePath -> PromotionInfo -> ResolvedApplication -> m ()
promote pkgPath repoPath prInfo res = do
    let name = prInfo ^. appName
        vOld = prInfo ^. oldVersion
    case prInfo ^. newVersion of
        Nothing -> liftIO $ putStrLn "No version to promote"
        Just vNew  -> case currentHost of
            Linux -> promoteLinux pkgPath repoPath name vOld vNew res
            _     -> promote'     pkgPath repoPath name vOld vNew res


run :: MonadPromote m => PromoteOpts -> m ()
run opts = do
    let cfgPath = convert $ opts ^. Opts.confPath :: FilePath
        pkgPath = convert $ opts ^. Opts.pkgPath  :: FilePath
        verType = if opts ^. Opts.toRelease then Release else Nightly

    pkgFullPath <- expand $ pkgPath
    cfgFullPath <- expand $ cfgPath
    prInfo      <- createNextVersion cfgFullPath verType Nothing

    config   <- Repository.parseConfig cfgFullPath
    resolved <- mapM (Repository.resolvePackageApp config) (config ^. Repository.apps)
    repo     <- Repository.getRepo

    forM_ resolved $ promote pkgFullPath (parent cfgFullPath) prInfo


    let updatedConfig = foldl' Repository.updateConfig config resolved
    Repository.generateConfigYamlWithNewPackage repo updatedConfig $ (parent pkgFullPath) </> "config.yaml"
