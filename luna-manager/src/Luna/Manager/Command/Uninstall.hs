{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Luna.Manager.Command.Uninstall where

import Prologue hiding (txt, FilePath, toText)

import qualified Control.Exception.Safe       as Exception
import qualified Data.Text                    as Text
import           Filesystem.Path.CurrentOS    (FilePath, (</>), decodeString, splitDirectories)
import qualified Safe
import qualified System.Directory             as Dir
import qualified System.Environment           as Environment

import           Control.Monad.State.Layered
import qualified Luna.Manager.Command.Install as Install
import           Luna.Manager.Command.Options (Options)
import qualified Luna.Manager.Command.Options as Options
import qualified Luna.Manager.Logger          as Logger
import           Luna.Manager.System          (stopServicesWindows)
import           Luna.Manager.System.Env
import           Luna.Manager.System.Host
import           Luna.Manager.System.Path
import qualified Luna.Manager.Shell.Shelly    as Shelly
import           Luna.Manager.Shell.Shelly    (MonadSh, MonadShControl)


default(Text.Text)

type MonadUninstall m = (MonadIO m, MonadSh m, MonadShControl m, MonadCatch m, MonadGetter Install.InstallConfig m,
                         MonadGetter Options m, MonadSetter Options m, MonadGetter EnvConfig m)

lunaStudio :: IsString s => s
lunaStudio = "LunaStudio"

uninstallServices :: MonadUninstall m => Install.InstallConfig -> m ()
uninstallServices opts = case currentHost of
    Windows -> do
        let currentServices = (opts ^. Install.defaultBinPathGuiApp)
                          </> lunaStudio
                          </> (opts ^. Install.selectedVersionPath)
                          </> (opts ^. Install.configPath)
                          </> fromText "windows"
        do
            testservices <- Shelly.test_d currentServices
            when testservices $ do
                Logger.log "Uninstalling Luna services"
                stopServicesWindows currentServices `Exception.catchAny` (\(e::SomeException) ->
                    Logger.warning $ "Uninstalling services failed "
                        <> "because of " <> convert (displayException e) <> ". Continuing...")
    _ -> return ()

guiDirectory :: FilePath -> FilePath
guiDirectory binPathGui = case currentHost of
    Windows -> binPathGui </> lunaStudio
    Linux   -> binPathGui
    Darwin  -> binPathGui </> lunaStudio

uninstallApp :: MonadUninstall m => Install.InstallConfig -> m ()
uninstallApp opts = do
    let defaultGuiPath = opts ^. Install.defaultBinPathGuiApp
        guiPath        = guiDirectory defaultGuiPath
    Logger.log $ "Removing directory " <> Shelly.toTextIgnore guiPath
    Shelly.rm_rf guiPath `Exception.catchAny` (\(e::SomeException) ->
        Logger.warning $ "Removing directory " <> Shelly.toTextIgnore guiPath <> " failed "
            <> "because of " <> convert (displayException e) <> ". Continuing...")

uninstallRunner :: MonadUninstall m => Install.InstallConfig -> m ()
uninstallRunner opts = case currentHost of
    Windows -> return ()
    _       -> do
        let localBinPath = (opts ^. Install.localBinPath) </> Shelly.fromText "luna-studio"
        expandedPath     <- expand localBinPath
        Logger.log "Removing symlink from ~/.local/bin"
        Shelly.rm_rf expandedPath `Exception.catchAny` (\(e::SomeException) ->
            Logger.warning $ "Removing symlink " <> Shelly.toTextIgnore expandedPath <> " failed "
                <> "because of " <> convert (displayException e) <> ". Continuing...")

uninstallLocalData :: MonadUninstall m => Install.InstallConfig -> m ()
uninstallLocalData opts = do
    confPath <- expand $ opts ^. Install.defaultConfPath
    Logger.log $ "Removing local config from " <> Shelly.toTextIgnore confPath
    Shelly.rm_rf confPath `Exception.catchAny` (\(e::SomeException) ->
        Logger.warning $ "Removing local config from " <> Shelly.toTextIgnore confPath <> " failed "
            <> "because of " <> convert (displayException e) <> ". Continuing...")

-- NOTE[MM]: splitting a directory and taking last is a bit more involved
--           than running Text.isInfixOf directly, but is more correct
--           if e.g. username is LunaStudio - in that case, every path
--           would be deleted
createdByLunaStudio :: FilePath -> Bool
createdByLunaStudio = (lunaStudio `Text.isInfixOf`) . Shelly.toTextIgnore . Safe.lastDef "" . splitDirectories

uninstallElectronCaches :: MonadUninstall m => m ()
uninstallElectronCaches = do
    baseDir <- case currentHost of
        Linux   -> decodeString <$> liftIO (Dir.getXdgDirectory Dir.XdgConfig "")
        Windows -> decodeString <$> liftIO (Dir.getAppUserDataDirectory "")
        Darwin  -> (\home -> decodeString home </> "Library" </> "Application Support") <$> liftIO Dir.getHomeDirectory
    dirs <- Shelly.ls baseDir
    -- see a NOTE in createdByLunaStudio
    let lunaStudioDirs = filter createdByLunaStudio dirs
    Logger.log $ "Removing Electron caches from " <> Shelly.toTextIgnore baseDir
    forM_ lunaStudioDirs $ \dir -> do
        Logger.log $ "    removing " <> Shelly.toTextIgnore dir
        Shelly.rm_rf dir `Exception.catchAny` (\(e::SomeException) ->
            Logger.warning $ "    removing " <> Shelly.toTextIgnore dir <> " failed "
            <> "because of " <> convert (displayException e) <> ". Continuing...")

uninstallStartMenuEntry :: MonadUninstall m => m ()
uninstallStartMenuEntry = case currentHost of
    Windows ->  do
        appdata <- Text.pack <$> liftIO (Environment.getEnv "appdata")
        Logger.log "Removing Luna Studio shortcut in Start Menu"
        let shortcut = Shelly.fromText appdata
                   </> "Microsoft"
                   </> "Windows"
                   </> "Start Menu"
                   </> "Programs"
                   </> "LunaStudio.lnk"
        Shelly.rm_rf shortcut `Exception.catchAny` (\(e::SomeException) ->
            Logger.warning $ "Removing Luna Studio shortcut in " <> Shelly.toTextIgnore shortcut <> " failed "
            <> "because of " <> convert (displayException e) <> ". Continuing...")
    Linux   -> do
        -- TODO[MM]: respect $XDG_DATA_HOME after a change in runner
        userDir <- (Shelly.fromText . Text.pack) <$> liftIO Dir.getHomeDirectory
        let desktopFilesDir = ".local/share/applications"
        desktops <- Shelly.ls $ userDir </> desktopFilesDir
        -- see a NOTE in createdByLunaStudio
        let lunaStudioFiles = filter createdByLunaStudio desktops
        Logger.log "Removing Luna Studio .desktop files from ~/.local/share/applications"
        forM_ lunaStudioFiles $ \desktop -> do
            Shelly.rm_rf desktop `Exception.catchAny` (\(e::SomeException) ->
                Logger.warning $ "Removing Luna Studio shortcut in " <> Shelly.toTextIgnore desktop <> " failed "
                <> "because of " <> convert (displayException e) <> ". Continuing...")
    _       -> return ()

run :: MonadUninstall m => m ()
run = do
    modify_ @Options $ Options.globals . Options.verbose .~ True
    conf <- get @Install.InstallConfig
    uninstallServices conf
    uninstallApp conf
    uninstallRunner conf
    uninstallLocalData conf
    uninstallElectronCaches
    uninstallStartMenuEntry
    Logger.log "Uninstalling Luna Studio completed"
