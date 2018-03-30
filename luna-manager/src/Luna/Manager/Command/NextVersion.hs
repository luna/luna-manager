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

import           Control.Monad.Raise
import           Luna.Manager.Component.Pretty
default (Text.Text)


type MonadNextVersion m = (MonadGetter Options m, MonadStates '[EnvConfig, RepoConfig] m, MonadNetwork m, Shelly.MonadSh m, Shelly.MonadShControl m, MonadIO m)

data VersionUpgradeException = VersionUpgradeException Text deriving Show

instance Exception VersionUpgradeException

data TargetVersionType = Dev | Nightly | Release deriving (Show, Eq)

data PromotionInfo = PromotionInfo { _appName     :: Text
                                   , _versionType :: TargetVersionType
                                   , _oldVersion  :: Version
                                   , _newVersion  :: Maybe Version
                                   , _commit      :: Maybe Text
                                   } deriving (Eq)
makeLenses ''PromotionInfo

instance Show PromotionInfo where
    show (PromotionInfo appName verType oldVer newVer commit) = header <> " " <> newInfo <> " " <> commitInfo
        where commitInfo = convert $ fromMaybe ""  $ ("from commit " <>) <$> commit
              header     = "[Promotion info -- " <> convert appName <>  "] Previous version: " <> (convert $ showPretty oldVer) <> "."
              newVerInfo = (": " <>) . convert . showPretty <$> newVer
              newInfo    = "Creating new " <> show verType <> " version" <> fromMaybe "" newVerInfo <> "."

getNewVersion :: PromotionInfo -> Either VersionUpgradeException Version
getNewVersion prInfo = case prInfo ^. newVersion of
    Just v  -> Right v
    Nothing -> Left $ VersionUpgradeException "Failed to construct the new version"

wrapException :: MonadNextVersion m => Either Text Version -> m (Either VersionUpgradeException Version)
wrapException = return . first VersionUpgradeException

readMaybeVersion :: Text -> Maybe Version
readMaybeVersion v = hush $ readPretty v

latestVersion :: (MonadNextVersion m,MonadIO m, MonadException SomeException m, MonadThrow m) => Text -> FilePath -> TargetVersionType -> m Version
latestVersion appName appPath targetVersionType = do
    let filterFunc = case targetVersionType of
            Release -> Version.isNightly
            Nightly -> Version.isDev
            Dev     -> const True
    Shelly.chdir appPath $ do
        tagsList <- Text.splitOn "\n" <$> Shelly.cmd "git" "tag" "-l"

        let vList = catMaybes $ readMaybeVersion <$> tagsList
        return $ case filter filterFunc (reverse vList) of
                (v:_) -> v
                _     -> def :: Version

nextVersion :: MonadNextVersion m => PromotionInfo -> m (Either VersionUpgradeException PromotionInfo)
nextVersion prInfo@(PromotionInfo _ targetVersionType latestVersion _ _) = do
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

tagVersion :: MonadNextVersion m => FilePath -> PromotionInfo -> m ()
tagVersion appPath prInfo = do
    version <- tryRight' $ getNewVersion prInfo
    let versionTxt  = showPretty version
        tagExists t = not . Text.null <$> Shelly.cmd "git" "tag" "-l" t
        tagSource   = if prInfo ^. versionType == Dev
                      then maybeToList $ prInfo ^. commit
                      else [showPretty $ prInfo ^. oldVersion]

    Shelly.chdir appPath $ Shelly.unlessM (tagExists versionTxt) $ do
        Shelly.run_ "git" (["tag", versionTxt] ++ tagSource)

createNextVersion :: MonadNextVersion m => FilePath -> TargetVersionType -> Maybe Text -> m PromotionInfo
createNextVersion cfgPath verType commitM = do
    let appPath = parent cfgPath
    config    <- Repo.parseConfig cfgPath
    name      <- tryRight' $ getAppName config
    latestVer <- latestVersion name appPath verType
    liftIO $ Text.putStrLn $ "The latest version is: " <> (showPretty latestVer)
    let promotionInfo = PromotionInfo { _appName     = name
                                      , _versionType = verType
                                      , _oldVersion  = latestVer
                                      , _newVersion  = Nothing
                                      , _commit      = commitM
                                      }

    newInfo <- nextVersion promotionInfo >>= tryRight'
    liftIO $ print newInfo
    tagVersion  appPath newInfo
    return newInfo

run :: MonadNextVersion m => NextVersionOpts -> m ()
run opts = do
    let cfgPath = convert $ opts ^. Opts.configFilePath :: FilePath
        verType = if opts ^. Opts.release then Release else if opts ^. Opts.nightly then Nightly else Dev

    void $ createNextVersion cfgPath verType $ opts ^. Opts.commit
