{-# LANGUAGE CPP #-}

module Luna.Manager.System.Env where

import Prologue hiding (FilePath, fromText, toText)

import           Luna.Manager.Command.Options
import           Luna.Manager.System.Host
import qualified Shelly.Lifted as Shelly
import           Shelly.Lifted (MonadSh)
import           Filesystem.Path.CurrentOS
import           Control.Monad.State.Layered
import qualified System.Directory as System
import           Control.Monad.Raise
import           System.IO.Error (isAlreadyExistsError)
import           System.IO.Temp  (createTempDirectory)

--------------------------
-- === EnvConfig === --
--------------------------

-- === Definition === --

data EnvConfig = EnvConfig { _localTempPath :: FilePath
                           }
makeLenses ''EnvConfig


-- === Utils === --

getHomePath :: MonadIO m => m FilePath
getHomePath = fromText . convert <$> liftIO System.getHomeDirectory

getCurrentPath :: MonadIO m => m FilePath
getCurrentPath = fromText . convert <$> liftIO System.getCurrentDirectory

getTmpPath, getDownloadPath :: (MonadIO m, MonadGetters '[Options, EnvConfig] m, MonadSh m) => m FilePath
getDownloadPath = getTmpPath
getTmpPath      = do
    userTmpPath <- gets @Options   (globals.selectedTmpPath)
    cfgTmpPath  <- gets @EnvConfig localTempPath
    let tmp = fromMaybe cfgTmpPath $ fromText <$> userTmpPath
    Shelly.mkdir_p tmp
    return tmp


setTmpCwd :: (MonadGetters '[Options, EnvConfig] m, MonadIO m, MonadSh m) => m ()
setTmpCwd = liftIO . System.setCurrentDirectory . encodeString =<< getTmpPath

createSymLink ::  MonadIO m => FilePath -> FilePath -> m ()
createSymLink src dst = liftIO $  (System.createFileLink (encodeString src) (encodeString dst)) `catch` handler src dst where
    --TODO[SB->WD]: Can we do it nicer - to check if it already exist (not the target!)
    handler :: FilePath -> FilePath -> SomeException -> IO ()
    handler src dst ex = do
        case fromException ex of
            Just ioExc -> if isAlreadyExistsError ioExc then do
                    System.removeFile $ encodeString dst
                    createSymLink src dst
                else return ()
            Nothing -> return ()

createSymLinkDirectory ::  MonadIO m => FilePath -> FilePath -> m ()
createSymLinkDirectory src dst = liftIO $ (System.createDirectoryLink (encodeString src) (encodeString dst)) `catch` handler src dst where
    handler :: FilePath -> FilePath -> SomeException -> IO ()
    handler src dst ex = do
        case fromException ex of
            Just ioExc -> if isAlreadyExistsError ioExc then do
                    System.removeDirectoryLink $ encodeString dst
                    createSymLinkDirectory src dst
                else return ()
            Nothing -> return ()


copyDir :: Shelly.MonadSh m => FilePath -> FilePath -> m ()-- copy the content of the source directory
copyDir src dst = do
    isDir <- Shelly.test_d src
    if isDir then do
        listedDirectory <- Shelly.ls src
        mapM_ (flip Shelly.cp_r dst) listedDirectory
    else Shelly.cp src dst


-- === Instances === --

instance {-# OVERLAPPABLE #-} MonadIO m => MonadHostConfig EnvConfig sys arch m where
    defaultHostConfig = do
        sysTmp  <- liftIO System.getTemporaryDirectory
        lunaTmp <- liftIO $ createTempDirectory sysTmp "luna"
        return $ EnvConfig $ decodeString lunaTmp

instance {-# OVERLAPPABLE #-} MonadIO m => MonadHostConfig EnvConfig 'Windows arch m where
    -- | Too long paths are often problem on Windows, therefore we use C:\tmp to store temporary data
    defaultHostConfig = EnvConfig <$> lunaTmp where
        lunaTmp = decodeString <$> (liftIO $ createTempDirectory "C:\\tmp" "luna")
