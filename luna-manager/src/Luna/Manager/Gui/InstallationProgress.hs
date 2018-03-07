--Warnig!! temporary file to refactor

module Luna.Manager.Gui.InstallationProgress where

import Prologue hiding (FilePath)

import Data.Aeson                    (FromJSON, ToJSON, FromJSONKey, ToJSONKey, parseJSON, encode)
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Encoding as JSON
import qualified Data.ByteString.Lazy as BS
import Control.Lens.Aeson
import System.IO (hFlush, stdout)


newtype InstallationProgress = InstallationProgress { installation_progress :: Float } deriving (Show, Generic)

instance ToJSON   InstallationProgress
instance FromJSON InstallationProgress

newtype ApplicationRun = ApplicationRun { application_run :: Text} deriving (Show, Generic)

instance ToJSON   ApplicationRun

newtype ApplicationClose = ApplicationClose { application_close :: Bool } deriving (Show, Generic)

instance ToJSON ApplicationClose

newtype Run = Run { run :: Text } deriving (Generic, Show)

instance ToJSON   Run
instance FromJSON Run

installationProgress :: MonadIO m => Float -> m ()
installationProgress pr = liftIO $ do
    print $ encode $ InstallationProgress pr
    hFlush stdout
