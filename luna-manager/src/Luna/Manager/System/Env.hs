{-# LANGUAGE CPP #-}

module Luna.Manager.System.Env where

import Prologue hiding (FilePath, fromText, toText)

import qualified Control.Monad.State.Layered as State
import qualified Shelly.Lifted               as Shelly
import qualified System.Directory            as System

import System.IO.Error (isAlreadyExistsError)
import System.IO.Temp  (createTempDirectory)

import Filesystem.Path.CurrentOS
import Luna.Manager.System.Host



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

getTmpPath, getDownloadPath :: (MonadIO m, State.Getter EnvConfig m) => m FilePath
getTmpPath      = State.gets @EnvConfig (view localTempPath)
getDownloadPath = getTmpPath

setTmpCwd :: (State.Getter EnvConfig m, MonadIO m) => m ()
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
                else pure ()
            Nothing -> pure ()

createSymLinkDirectory ::  MonadIO m => FilePath -> FilePath -> m ()
createSymLinkDirectory src dst = liftIO $ (System.createDirectoryLink (encodeString src) (encodeString dst)) `catch` handler src dst where
    handler :: FilePath -> FilePath -> SomeException -> IO ()
    handler src dst ex = do
        case fromException ex of
            Just ioExc -> if isAlreadyExistsError ioExc then do
                    System.removeDirectoryLink $ encodeString dst
                    createSymLinkDirectory src dst
                else pure ()
            Nothing -> pure ()


copyDir :: Shelly.MonadSh m => FilePath -> FilePath -> m ()-- copy the content of the source directory
copyDir src dst = do
    isDir <- Shelly.test_d src
    if isDir then do
        listedDirectory <- Shelly.ls src
        mapM_ (flip Shelly.cp_r dst) listedDirectory
    else Shelly.cp src dst


-- === Instances === --

instance {-# OVERLAPPABLE #-} MonadIO m => MonadHostConfig EnvConfig sys arch m where
    defaultHostConfig = liftIO $ do
        sysTmp  <- System.getTemporaryDirectory
        lunaTmp <- createTempDirectory sysTmp "luna"
        pure $ EnvConfig $ decodeString lunaTmp
