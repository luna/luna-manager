{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Command.NextVersion where

import Prologue                                    hiding (FilePath)

import           Control.Exception.Base            (Exception, throwIO)
import           Control.Monad.Raise               (tryRight')
import           Control.Monad.State.Layered
import           Data.Bifunctor                    (first, second)
import qualified Data.Map                          as Map
import           Data.Maybe                        (maybeToList)
import qualified Data.Text                         as Text
import qualified Data.Text.IO                      as Text
import           Filesystem.Path.CurrentOS         (FilePath, parent)

import           Luna.Manager.Command.Options      (Options, NextVersionOpts)
import qualified Luna.Manager.Command.Options      as Opts
import           Luna.Manager.Component.Pretty     (showPretty)
import           Luna.Manager.Component.Repository as Repo
import qualified Luna.Manager.Component.Version    as Version
import           Luna.Manager.Component.Version    (Version, VersionInfo)
import           Luna.Manager.Network
import qualified Luna.Manager.Shell.Shelly         as Shelly
import           Luna.Manager.System.Env

default (Text.Text)


type MonadNextVersion m = (MonadGetter Options m, MonadStates '[EnvConfig, RepoConfig] m, MonadNetwork m, Shelly.MonadSh m, Shelly.MonadShControl m, MonadIO m)

data VersionUpgradeException = VersionUpgradeException Text deriving Show

instance Exception VersionUpgradeException

data TargetVersionType = Dev | Nightly | Release deriving (Show, Eq)

data PromotionInfo = PromotionInfo { _versionType :: TargetVersionType
                                   , _oldVersion  :: Version
                                   , _newVersion  :: Maybe Version
                                   , _commit      :: Maybe Text
                                   } deriving (Eq)
makeLenses ''PromotionInfo

instance Show PromotionInfo where
    show (PromotionInfo verType oldVer newVer commit) = header <> " " <> newInfo <> " " <> commitInfo
        where commitInfo = convert $ fromMaybe ""  $ ("from commit " <>) <$> commit
              header     = "[Promotion info] Previous version: " <> (convert $ showPretty oldVer) <> "."
              newVerInfo = (": " <>) . convert . showPretty <$> newVer
              newInfo    = "Creating new " <> (show verType) <> " version" <> (fromMaybe "" newVerInfo) <> "."

getNewVersion :: PromotionInfo -> Either VersionUpgradeException Version
getNewVersion prInfo = case prInfo ^. newVersion of
    Just v  -> Right v
    Nothing -> Left $ VersionUpgradeException "Failed to construct the new version"

wrapException :: MonadNextVersion m => Either Text Version -> m (Either VersionUpgradeException Version)
wrapException = return . first VersionUpgradeException

latestVersion :: MonadNextVersion m => Text -> TargetVersionType -> m Version
latestVersion appName targetVersionType = do
    let filterFunc = case targetVersionType of
            Release -> Version.isNightly
            Nightly -> Version.isDev
            Dev     -> const True
    repo     <- Repo.getRepo
    versions <- Repo.getVersionsList repo appName
    return $ case filter filterFunc versions of
            (v:_) -> v
            _     -> def :: Version

nextVersion :: MonadNextVersion m => PromotionInfo -> m (Either VersionUpgradeException PromotionInfo)
nextVersion prInfo@(PromotionInfo targetVersionType latestVersion _ _) = do
    let next = case targetVersionType of
            Dev     -> Right . Version.nextBuild
            Nightly -> Version.promoteToNightly
            Release -> Version.promoteToRelease
    versionE <- wrapException $ next latestVersion
    return $ second (\v -> prInfo & newVersion ?~ v) versionE

getAppName :: Repo.Repo -> Either VersionUpgradeException Text
getAppName cfg = case cfg ^? apps . ix 0 of
    Just app -> Right app
    Nothing  -> Left $ VersionUpgradeException "Unable to determine the app to upgrade."

saveVersion :: MonadNextVersion m => Text -> Text -> PromotionInfo -> m ()
saveVersion cfgFile appName prInfo = do
    config  <- Repo.parseConfig $ convert cfgFile
    version <- tryRight' $ getNewVersion prInfo
    let newConfig = config & packages . ix appName . versions %~ Map.mapKeys (\_ -> version)
    Repo.saveYamlToFile newConfig $ convert cfgFile

commitVersion :: MonadNextVersion m => Text -> PromotionInfo -> m ()
commitVersion cfgFile prInfo = do
    version <- tryRight' $ getNewVersion prInfo
    let msg = "New version: " <> (showPretty version)
    Shelly.chdir (parent $ fromText cfgFile) $ Shelly.cmd "git" "commit" "-am" msg

tagVersion :: MonadNextVersion m => FilePath -> PromotionInfo -> m ()
tagVersion appPath prInfo = do
    version <- tryRight' $ getNewVersion prInfo
    let versionTxt  = showPretty version
        tagExists t = not . Text.null <$> Shelly.cmd "git" "tag" "-l" t
        tagSource   = if prInfo ^. versionType == Dev
                      then maybeToList $ prInfo ^. commit
                      else [showPretty $ prInfo ^. oldVersion]

    Shelly.chdir appPath $ Shelly.unlessM (tagExists versionTxt)
                         $ Shelly.run_ "git" (["tag", versionTxt] ++ tagSource)

run :: MonadNextVersion m => NextVersionOpts -> m ()
run opts = do
    let cfgPath = opts ^. Opts.configFilePath
        appPath = parent $ convert cfgPath
        verType = if opts ^. Opts.release then Release else if opts ^. Opts.nightly then Nightly else Dev

    config    <- Repo.parseConfig $ convert cfgPath
    appName   <- tryRight' $ getAppName config
    latestVer <- latestVersion appName verType
    liftIO $ Text.putStrLn $ "The latest version is: " <> (showPretty latestVer)
    let promotionInfo = PromotionInfo { _versionType = verType
                                      , _oldVersion  = latestVer
                                      , _newVersion  = Nothing
                                      , _commit      = opts ^. Opts.commit
                                      }

    newInfo <- nextVersion promotionInfo >>= tryRight'
    liftIO $ print newInfo
    saveVersion   cfgPath appName newInfo
    commitVersion cfgPath newInfo
    tagVersion    appPath newInfo

