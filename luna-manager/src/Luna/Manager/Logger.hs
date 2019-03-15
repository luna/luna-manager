{-# LANGUAGE OverloadedStrings #-}

module Luna.Manager.Logger where

import qualified Control.Monad.State.Layered as State
import qualified Data.Text.IO                as Text
import qualified Shelly.Lifted               as Sh
import qualified System.Process.Typed        as Process

import Control.Monad.Exception      (MonadException, throw)
import Data.Aeson                   (FromJSON, ToJSON, encode)
import Data.Text                    (Text, pack, unpack)
import Filesystem.Path.CurrentOS    (FilePath, decodeString, (</>))
import Luna.Manager.Command.Options (Options, globals, guiInstaller, verbose)
import Luna.Manager.System.Env      (EnvConfig)
import Prologue                     hiding (FilePath, log)
import Shelly.Lifted                (MonadSh, MonadShControl)
import System.Directory             (getAppUserDataDirectory)
import System.IO                    (hFlush, stdout)




data WarningMessage = WarningMessage { message :: Text
                                     } deriving (Show, Generic, Eq)

instance ToJSON   WarningMessage
instance FromJSON WarningMessage

type LoggerMonad m = (MonadIO m, MonadSh m, MonadShControl m, State.Getters '[Options, EnvConfig] m)


logFilePath :: LoggerMonad m => m FilePath
logFilePath = do
    tmpDir <- decodeString <$> (liftIO $ getAppUserDataDirectory "luna_manager")
    Sh.mkdir_p tmpDir
    pure $ tmpDir </> "luna-manager.log"

logToStdout :: Text -> IO ()
logToStdout msg = do
    Text.putStrLn msg
    hFlush stdout

logToFile :: FilePath -> Text -> IO ()
logToFile fp msg = Sh.shelly $ Sh.appendfile fp msg

logToTmpFile :: LoggerMonad m => Text -> m ()
logToTmpFile msg = do
    fp <- logFilePath
    Sh.appendfile fp msg

info :: LoggerMonad m => Text -> m ()
info msg = do
    opts <- view globals <$> State.get @Options
    let gui  = opts ^. guiInstaller
        msg' = msg <> "\n"
    if gui then logToTmpFile msg' else liftIO $ logToStdout msg'

log :: LoggerMonad m => Text -> m ()
log msg = do
    -- TODO[piotrMocz] we need a more robust logging solution in the long run
    opts <- view globals <$> State.get @Options
    let verb = opts ^. verbose
        gui  = opts ^. guiInstaller
        msg' = msg <> "\n"
    if verb && (not gui) then liftIO $ logToStdout msg' else logToTmpFile msg'

logProcess :: LoggerMonad m => Text -> m ()
logProcess cmd = do
    logToTmpFile $ cmd <> "\n"
    let proc = Process.readProcess . Process.shell
    (exit, out, err) <- proc $ unpack cmd
    logToTmpFile $ "output: " <> pack (show out) <> "\n"
    logToTmpFile $ "error: "  <> pack (show err) <> "\n"

logToJSON :: MonadIO m => Text -> m ()
logToJSON = liftIO . print . encode . WarningMessage

warning :: LoggerMonad m => Text -> m ()
warning msg = do
    opts <- view globals <$> State.get @Options
    let verb = opts ^. verbose
        gui  = opts ^. guiInstaller
        m    = "WARNING: " <> msg
    if   gui     then logToJSON m
    else do
        liftIO $ logToStdout m
        logToTmpFile m

exception :: (LoggerMonad m, Show e) => Text -> e -> m ()
exception funName exc = log $ "[Exception in " <> funName <> "] " <> (pack $ show exc)

logObject :: (LoggerMonad m, Show a) => Text -> a -> m ()
logObject name obj = log $ name <> ": " <> (pack $ show obj)

tryJustWithLog :: (LoggerMonad m, Show e, MonadException e m) => Text -> e -> Maybe a -> m a
tryJustWithLog funName e (Just x) = pure x
tryJustWithLog funName e Nothing  = exception funName e >> throw e
