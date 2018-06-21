{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Luna.Manager.Command.Develop where

import Prologue hiding (FilePath)
import Luna.Manager.Command.Options
import Luna.Manager.Network
import Luna.Manager.System.Env
import Control.Monad.Raise
import Control.Monad.State.Layered
import Data.Text                   (Text)
import Luna.Manager.Shell.Shelly   (MonadSh, MonadShControl)
import Filesystem.Path.CurrentOS   (FilePath, (</>), encodeString, decodeString, toText, basename, hasExtension, parent)
import qualified System.Directory as System
import qualified Luna.Manager.Shell.Shelly as Shelly
import qualified Luna.Manager.Archive      as Archive

import Luna.Manager.Component.Repository
import Luna.Manager.Command.CreatePackage
import Luna.Manager.Component.Version
import Luna.Manager.System.Host
import Luna.Manager.System.Path (expand)
import Control.Monad.Trans.Resource ( MonadBaseControl)

import qualified Data.Text as T
default (T.Text)



data DevelopConfig = DevelopConfig { _stackPath      :: Text
                                   , _devPath        :: FilePath
                                   , _appsPath       :: FilePath
                                   , _toolsPath      :: FilePath
                                   , _stackLocalPath :: FilePath
                                   , _bootstrapFile  :: FilePath
                                    }
makeLenses ''DevelopConfig

type MonadDevelop m = (MonadGetter Options m, MonadStates '[EnvConfig, RepoConfig, PackageConfig, DevelopConfig] m, MonadIO m, MonadException SomeException m, MonadSh m, MonadShControl m, MonadCatch m, MonadBaseControl IO m)


instance Monad m => MonadHostConfig DevelopConfig 'Linux arch m where
    defaultHostConfig = return $ DevelopConfig
        { _stackPath      = "https://github.com/commercialhaskell/stack/releases/download/v1.6.3/stack-1.6.3-linux-x86_64-static.tar.gz"
        , _devPath        = "luna-develop"
        , _appsPath       = "apps"
        , _toolsPath      = "tools"
        , _stackLocalPath = "stack"
        , _bootstrapFile  = "bootstrap.hs"
        }

instance Monad m => MonadHostConfig DevelopConfig 'Darwin arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & stackPath .~ "https://github.com/commercialhaskell/stack/releases/download/v1.6.3/stack-1.6.3-osx-x86_64.tar.gz"

instance Monad m => MonadHostConfig DevelopConfig 'Windows arch m where
    defaultHostConfig = defaultHostConfigFor @Linux

data PathException = PathException deriving (Show)
instance Exception PathException where
    displayException exception = "Can not download dependencies without repository path."

downloadAndUnpackStack :: MonadDevelop m => FilePath -> m ()
downloadAndUnpackStack path = do
    stackPresent <- Shelly.test_d path
    if stackPresent
        then liftIO $ putStrLn "Stack is already installed, skipping"
        else do
            developConfig <- get @DevelopConfig
            let stackURL           = developConfig ^. stackPath
                totalProgress      = 1.0
                progressFielsdName = ""
            putStrLn "Downloading stack"
            stackArch <- downloadWithProgressBar stackURL
            stackArch' <- Archive.unpack totalProgress progressFielsdName stackArch
            Shelly.whenM (Shelly.test_d path) $ Shelly.rm_rf path
            Shelly.mv stackArch' path

getLatestRepo :: MonadDevelop m => Text -> FilePath -> m Text
getLatestRepo appName appPath = do
    let repoPath = "git@github.com:luna/" <> appName <> ".git"
    repoExists <- Shelly.test_d appPath
    if repoExists
        then Shelly.chdir appPath $ Shelly.cmd "git" "pull" "origin" "master"
        else Shelly.cmd "git" "clone" repoPath appPath

downloadDeps :: MonadDevelop m => Text -> FilePath -> m ()
downloadDeps appName appPath = do
    let yamlPath = appPath </>  "luna-package.yaml"
    config <- parseConfig yamlPath
    resolvedApplication <- resolvePackageApp config appName
    mapM_ (downloadAndUnpackDependency appPath) $ resolvedApplication ^. pkgsToPack


run :: MonadDevelop m => DevelopOpts -> m ()
run opts = do
    developCfg <- get @DevelopConfig
    let appName  = opts ^. target
    if (opts ^. downloadDependencies) then do
        path <- tryJust (toException PathException) (opts ^. repositoryPath)
        downloadDeps appName $ convert path
    else do
        let path = opts ^. repositoryPath
        workingPath <- case path of
            Just workingPath -> return workingPath
            Nothing -> do
                home <- liftIO $ System.getHomeDirectory
                return $ convert home

        devRootPath   <- workingPath </> (developCfg ^. devPath)
        stackRootPath <- devRootPath </> ".stack"
        stackFolder   <- (developCfg ^. toolsPath) </> (developCfg ^. stackLocalPath)
        appPath       <- (developCfg ^. appsPath)  </> (convert appName)

        stackRootVar   <- expand $ convert stackRootPath
        appPathVar     <- expand $ convert devRootPath   </> appPath
        stackFolderVar <- expand $ convert devRootPath   </> stackFolder
        stackBinVar    <- expand $ convert stackRootPath </> "bin"

        Shelly.mkdir_p $ parent stackFolderVar
        downloadAndUnpackStack stackFolderVar
        getLatestRepo appName appPathVar

        Shelly.prependToPath stackFolderVar
        Shelly.prependToPath stackBinVar

        Shelly.setenv "LUNA_DEV_ROOT" $ Shelly.toTextIgnore devRootPath
        Shelly.setenv "APP_PATH"      $ Shelly.toTextIgnore appPathVar

        let bootstrapPath      = Shelly.toTextIgnore $ appPathVar </> (developCfg ^. bootstrapFile)
            bootstrapPackages  = ["base", "exceptions", "shelly", "text", "directory", "system-filepath"]
            bootstrapStackArgs = ["--resolver", "lts-8.2", "--install-ghc" , "runghc"]
                                 <> (bootstrapPackages >>= (\p -> ["--package", p]))
                                 <> [bootstrapPath]
        Shelly.run "stack" bootstrapStackArgs
        downloadDeps appName appPath
