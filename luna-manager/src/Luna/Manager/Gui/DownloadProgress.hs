module Luna.Manager.Gui.DownloadProgress where

import Prelude

import GHC.Generics
import Control.Monad.IO.Class

import Data.Aeson                    (FromJSON, ToJSON, FromJSONKey, ToJSONKey, parseJSON, encode)
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Encoding as JSON
import System.IO (hFlush, stdout)

data Progress = Progress { completed :: Int
                         , total     :: Int
                         }

data DownloadProgress = DownloadProgress { download_progress :: Float} deriving (Generic, Show)

instance ToJSON   DownloadProgress
instance FromJSON DownloadProgress

progress :: MonadIO m => Progress -> m ()
progress (Progress completed total) = liftIO $ do
    print $ encode $ DownloadProgress pr
    liftIO $ hFlush stdout
    where
        pr = fromIntegral completed / fromIntegral total