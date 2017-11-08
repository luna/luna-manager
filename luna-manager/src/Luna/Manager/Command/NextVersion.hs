module Luna.Manager.Command.NextVersion where

import Prologue

import           Control.Exception.Base            (Exception, throwIO)
import           Control.Monad.Raise               (tryRight')
import           Control.Monad.State.Layered
import           Data.Bifunctor                    (first)
import qualified Data.Map                          as Map
import qualified Data.Text.IO                      as Text

import           Luna.Manager.Command.Options      (Options, NextVersionOpts)
import qualified Luna.Manager.Command.Options      as Opts
import           Luna.Manager.Component.Pretty     (showPretty)
import           Luna.Manager.Component.Repository as Repo
import qualified Luna.Manager.Component.Version    as Version
import           Luna.Manager.Component.Version    (Version, VersionInfo)
import           Luna.Manager.Network
import qualified Luna.Manager.Shell.Shelly         as Shelly
import           Luna.Manager.System.Env


type MonadNextVersion m = (MonadGetter Options m, MonadStates '[EnvConfig, RepoConfig] m, MonadNetwork m, Shelly.MonadSh m, Shelly.MonadShControl m, MonadIO m)

data VersionUpgradeException = VersionUpgradeException Text deriving Show

instance Exception VersionUpgradeException

wrapException :: MonadNextVersion m => Either Text Version -> m (Either VersionUpgradeException Version)
wrapException = return . first VersionUpgradeException

currentVersion :: MonadNextVersion m => Maybe String -> m Version
currentVersion mTag = do
    let appName    = "luna-studio"
        filterFunc = case mTag of
            Just "release" -> Version.isRelease
            Just "nightly" -> Version.isNightly
            Just "build"   -> Version.isBuild
            _              -> const True
    repo     <- Repo.getRepo
    versions <- Repo.getVersionsList repo appName
    return $ case filter filterFunc versions of
            (v:_) -> v
            _     -> def :: Version

nextBuild :: MonadNextVersion m => m (Either VersionUpgradeException Version)
nextBuild = do
    currVer <- currentVersion Nothing
    liftIO $ print $ "The latest version for luna-studio is: " <> (showPretty currVer)
    wrapException $ Right $ Version.nextBuild currVer

promoteToNightly :: MonadNextVersion m => m (Either VersionUpgradeException Version)
promoteToNightly = do
    currVer <- currentVersion $ Just "build"
    liftIO $ print $ "The latest build version for luna-studio is: " <> (showPretty currVer)
    wrapException $ Version.promoteToNightly currVer

promoteToRelease :: MonadNextVersion m => m (Either VersionUpgradeException Version)
promoteToRelease = do
    currVer <- currentVersion $ Just "nightly"
    liftIO $ print $ "The latest nightly version for luna-studio is: " <> (showPretty currVer)
    wrapException $ Version.promoteToRelease currVer

getAppName :: Repo.Repo -> Either VersionUpgradeException Text
getAppName cfg = case cfg ^? apps . ix 0 of
    Just app -> Right app
    Nothing  -> Left $ VersionUpgradeException "Unable to determine the app to upgrade."

saveVersion :: MonadNextVersion m => Text -> Version -> m ()
saveVersion cfgFile v = do
    config  <- Repo.parseConfig $ convert cfgFile
    appName <- tryRight' $ getAppName config
    let newConfig = config & packages . ix appName . versions %~ Map.mapKeys (\_ -> v)
    liftIO $ Text.putStrLn $ "New version: " <> (showPretty v)
    Repo.saveYamlToFile newConfig $ convert cfgFile

run :: MonadNextVersion m => NextVersionOpts -> m ()
run opts = do
    v      <- if      opts ^. Opts.nightly then promoteToNightly
              else if opts ^. Opts.release then promoteToRelease
              else                              nextBuild
    saveVersion (opts ^. Opts.configFilePath) =<< tryRight' v
