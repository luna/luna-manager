module Luna.Manager.Logger where

import           Prologue                     hiding (FilePath)
import           Control.Monad.State.Layered
import           Data.Text                    (Text)
import qualified Data.Text.IO                 as Text
import           Filesystem.Path.CurrentOS    (FilePath, (</>), decodeString)
import           Shelly.Lifted                (MonadSh, MonadShControl)
import qualified Shelly.Lifted                as Sh
import           System.IO                    (hFlush, stdout)
import qualified System.Directory     as SystemDirectory

import           Data.Aeson           (FromJSON, ToJSON, FromJSONKey, ToJSONKey, parseJSON, encode)
import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Types     as JSON
import qualified Data.Aeson.Encoding  as JSON
import qualified Data.ByteString.Lazy as BS
import           Control.Lens.Aeson

import           Luna.Manager.Command.Options
import           Luna.Manager.System.Env      (EnvConfig)
import qualified Luna.Manager.System.Env      as System


data WarningMessage = WarningMessage { message :: Text
                                     } deriving (Show, Generic, Eq)

instance ToJSON   WarningMessage
instance FromJSON WarningMessage

type LoggerMonad m = (MonadIO m, MonadSh m, MonadShControl m, MonadGetters '[Options, EnvConfig] m)


logFilePath :: LoggerMonad m => m FilePath
logFilePath = do
    tmpDir <- liftIO $ SystemDirectory.getTemporaryDirectory
    return $ (decodeString tmpDir) </> "luna-manager.log"

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

log :: LoggerMonad m => Text -> m ()
log msg = do
    -- TODO[piotrMocz] we need a more robust logging solution in the long run
    opts <- view globals <$> get @Options
    let verb = opts ^. verbose
        gui  = opts ^. guiInstaller
    if verb && (not gui) then liftIO $ logToStdout msg else logToTmpFile msg

logToJSON :: MonadIO m => Text -> m ()
logToJSON = liftIO . print . encode . WarningMessage

warning :: LoggerMonad m => Text -> m ()
warning msg = do
    opts <- view globals <$> get @Options
    let verb = opts ^. verbose
        gui  = opts ^. guiInstaller
        m    = "WARNING: " <> msg
    if   gui     then logToJSON m
    else if verb then liftIO $ logToStdout m
    else              logToTmpFile m
