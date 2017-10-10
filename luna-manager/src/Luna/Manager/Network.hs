module Luna.Manager.Network where

import Prologue hiding (FilePath, fromText)

import Luna.Manager.Command.Options (Options, guiInstallerOpt)
import Luna.Manager.System.Env
import Luna.Manager.Shell.ProgressBar
import Luna.Manager.System.Path
import Luna.Manager.Shell.Shelly (MonadSh)

import Control.Monad.Raise
import Control.Monad.State.Layered
import Filesystem.Path.CurrentOS
import qualified Network.HTTP.Conduit       as HTTP
import           Network.HTTP.Conduit       (httpLbs)
import qualified Network.URI                as URI
import qualified Data.ByteString.Lazy.Char8 as ByteStringL

import Control.Monad.Trans.Resource ( MonadBaseControl, runResourceT)

import Data.Conduit (($$+-),($=+))
import Data.Conduit.List (sinkNull)
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Types (hContentLength)
import qualified Data.ByteString.Char8 as ByteStringChar (unpack, writeFile)
import qualified Data.Text as Text
import qualified Control.Exception.Safe as Exception

-- === Errors === --

data DownloadException = DownloadException Text SomeException deriving (Show)
instance Exception DownloadException where
    displayException (DownloadException file exception) = "Couldn't download file: " <> convert file <> " because of: "  <> displayException exception

-- downloadError :: SomeException
-- downloadError = toException DownloadError


-- === Utils === --

takeFileNameFromURL :: URIPath -> Maybe Text
takeFileNameFromURL url = convert <$> name where
    name = maybeLast . URI.pathSegments =<< URI.parseURI (convert url)

type MonadNetwork m = (MonadIO m, MonadState Options m, MonadGetter EnvConfig m, MonadException SomeException m, MonadSh m, MonadCatch m, MonadThrow m,  MonadBaseControl IO m)

downloadFromURL :: MonadNetwork m => URIPath -> Text -> m FilePath
downloadFromURL address info = do
    guiInstaller <- guiInstallerOpt
    let go = withJust (takeFileNameFromURL address) $ \name -> do
            unless guiInstaller $ putStrLn $ (convert info) <>" (" <> convert address <> ")"
            dest    <- (</> (fromText name)) <$> getDownloadPath
            manager <- newHTTPManager
            request <- HTTP.parseUrlThrow (convert address)
            resp    <- httpLbs request manager
            liftIO $ ByteStringL.writeFile (encodeString dest) $ HTTP.responseBody resp
            return dest
    go `Exception.catchAny` \e -> throwM (DownloadException address e)  where

newHTTPManager :: MonadIO m => m HTTP.Manager
newHTTPManager = liftIO . HTTP.newManager $ HTTP.tlsManagerSettings { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 5000000}

downloadWithProgressBar  :: MonadNetwork m => URIPath -> m FilePath
downloadWithProgressBar address = do
    guiInstaller <- guiInstallerOpt
    unless guiInstaller $ putStrLn $ "Downloading: " <> (convert address)
    tmp <- getTmpPath
    downloadWithProgressBarTo address tmp

downloadWithProgressBarTo :: MonadNetwork m => URIPath -> FilePath -> m FilePath
downloadWithProgressBarTo address dstPath = Exception.handleAny (\e -> throwM (DownloadException address e)) $  do
    guiInstaller <- guiInstallerOpt
    req     <- HTTP.parseRequest (convert address)
    manager <- newHTTPManager
    runResourceT $ do
    -- Start the request
        withJust (takeFileNameFromURL address) $ \name -> do
            let dstFile = dstPath </> (fromText name)
            res <- HTTP.http req manager
            -- Get the Content-Length and initialize the progress bar
            let Just cl  = lookup hContentLength (HTTP.responseHeaders res)
                pgTotal  = read (ByteStringChar.unpack cl)
                pg       = ProgressBar 50 0 pgTotal
                progress = Progress 0 pgTotal
            -- Consume the response updating the progress bar
            if guiInstaller then
                HTTP.responseBody res $=+ updateProgress progress $$+- sinkFile (encodeString dstFile)
                else do
                    HTTP.responseBody res $=+ updateProgressBar pg $$+- sinkFile (encodeString dstFile)
                    putStrLn "Download completed!"
            return dstFile

-- downloadWithProgressBarAndUnpack :: (MonadIO m, MonadException SomeException m, MonadGetter EnvConfig m) => URIPath -> m FilePath
-- downloadWithProgressBarAndUnpack address = do
--     tmp <- getTmpPath
--     print =<< downloadWithProgressBar address tmp
--     return undefined
