{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Luna.Manager.Command.Uninstall where

import Prologue hiding (txt, FilePath, toText)

import qualified Control.Exception.Safe       as Exception
import qualified Data.Text                    as Text
import           Filesystem.Path.CurrentOS    (FilePath, (</>))

import           Control.Monad.State.Layered
import qualified Luna.Manager.Command.Install as Install
import           Luna.Manager.Command.Options (Options)
import qualified Luna.Manager.Logger          as Logger
import           Luna.Manager.System          (stopServicesWindows)
import           Luna.Manager.System.Env
import           Luna.Manager.System.Host
import           Luna.Manager.System.Path
import qualified Luna.Manager.Shell.Shelly    as Shelly
import           Luna.Manager.Shell.Shelly    (MonadSh, MonadShControl)


default(Text.Text)

type MonadUninstall m = (MonadIO m, MonadSh m, MonadShControl m, MonadCatch m, MonadGetter Install.InstallConfig m, MonadGetter Options m, MonadGetter EnvConfig m)

uninstallServices :: MonadUninstall m => Install.InstallConfig -> m ()
uninstallServices opts = case currentHost of
    Windows -> do
        let currentServices = (opts ^. Install.defaultBinPathGuiApp)
                          </> Shelly.fromText "LunaStudio"
                          </> Shelly.fromText "current"
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
    Windows -> binPathGui </> "LunaStudio"
    Linux   -> binPathGui
    Darwin  -> binPathGui </> "LunaStudio"

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

run :: MonadUninstall m => m ()
run = do
    conf <- get @Install.InstallConfig
    uninstallServices conf
    uninstallApp conf
    uninstallRunner conf
    uninstallLocalData conf
    Logger.log "Uninstalling Luna Studio completed"
