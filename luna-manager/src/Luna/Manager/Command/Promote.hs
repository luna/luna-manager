{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Command.Promote where

import Prologue hiding (FilePath, (<.>))

import           Control.Exception.Safe            as Exception
import           Control.Monad.State.Layered
import qualified Data.Text                         as Text
import           Filesystem.Path.CurrentOS         (FilePath, parent, encodeString, fromText, (</>), (<.>), filename)

import qualified Luna.Manager.Archive              as Archive
import           Luna.Manager.Command.NextVersion  (PromotionInfo(..), TargetVersionType(..), VersionUpgradeException(..), createNextVersion, newVersion, appName)
import           Luna.Manager.Command.Options      (Options, NextVersionOpts, PromoteOpts)
import qualified Luna.Manager.Command.Options      as Opts
import qualified Luna.Manager.Logger               as Logger
import           Luna.Manager.Network              (MonadNetwork, downloadWithProgressBarTo)
import           Luna.Manager.Component.Pretty     (showPretty)
import           Luna.Manager.Component.Repository (RepoConfig)
import           Luna.Manager.Component.Version    (Version)
import qualified Luna.Manager.Shell.Shelly         as Shelly
import           Luna.Manager.System               (makeExecutable)
import           Luna.Manager.System.Env
import           Luna.Manager.System.Host          (currentHost, System(..))

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


renameVersion :: MonadPromote m => FilePath -> Version -> m ()
renameVersion path version = do
    let prettyVersion =  showPretty version
        versionFile   =  encodeString $ path </> "config" </> "version.txt"

    Logger.log $ "Writing new version number: " <> prettyVersion
    liftIO $ writeFile versionFile (convert prettyVersion)


promote' :: MonadPromote m => FilePath -> Text -> Version -> m ()
promote' pkgPath name version = do
    Logger.log "Unpacking the package"
    extracted <- Archive.unpack 1.0 "unpacking_progress" pkgPath

    renameVersion extracted version

    let correctPath = (parent extracted) </> (convert name)
    Logger.log $ "Renaming " <> (Shelly.toTextIgnore extracted) <> " to " <> (Shelly.toTextIgnore correctPath)
    Shelly.mv extracted correctPath `Exception.catchAny` (\(e :: SomeException) ->
        Logger.warning $ "Failed to rename the extracted folder.\n" <> (convert $ displayException e))

    Logger.log $ "Compressing the package"
    let newName = newPackageName pkgPath version
    compressed <- Archive.pack correctPath newName

    Logger.log "Cleaning up"
    Shelly.rm_rf correctPath `Exception.catchAny` (\(e :: SomeException) ->
        Logger.warning $ "Failed to clean up after extracting.\n" <> (convert $ displayException e))


promoteLinux :: MonadPromote m => FilePath -> Text -> Version -> m ()
promoteLinux pkgPath name version = do
    Logger.log "Unpacking AppImage"
    Shelly.cmd pkgPath "--appimage-extract"

    let appDir = "squashfs-root" :: FilePath
    renameVersion (appDir </> "usr") version

    Logger.log "Downloading appImageTool"
    let aitUrl = "https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage"
    appImageTool <- downloadWithProgressBarTo aitUrl "."
    makeExecutable appImageTool

    -- TODO: change the version in the name once it lands
    Logger.log "Repacking AppImage"
    let aiName    = Shelly.toTextIgnore $ filename pkgPath
        aiNewName = newPackageName pkgPath version <> ".AppImage"
    Shelly.cmd appImageTool appDir aiNewName

    Logger.log "Cleaning up"
    Shelly.rm_rf appDir `Exception.catchAny` (\(e :: SomeException) ->
        Logger.warning $ "Failed to clean up after extracting.\n" <> (convert $ displayException e))


promote :: MonadPromote m => FilePath -> PromotionInfo -> m ()
promote pkgPath prInfo = do
    let name = prInfo ^. appName
    case prInfo ^. newVersion of
        Nothing -> liftIO $ putStrLn "No version to promote"
        Just v  -> case currentHost of
            Linux -> promoteLinux pkgPath name v
            _     -> promote'     pkgPath name v


run :: MonadPromote m => PromoteOpts -> m ()
run opts = do
    let cfgPath = convert $ opts ^. Opts.confPath :: FilePath
        pkgPath = convert $ opts ^. Opts.pkgPath  :: FilePath
        verType = if opts ^. Opts.toRelease then Release else Nightly

    prInfo <- createNextVersion cfgPath verType Nothing
    promote pkgPath prInfo
