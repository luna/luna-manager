--Warnig!! temporary file to refactor

module Luna.Manager.Gui.Initialize where

import Prologue hiding (FilePath, (.=), (.:))
import qualified Data.Map as Map
import Luna.Manager.Component.Repository
import Luna.Manager.Component.Version
import Luna.Manager.System.Host
import Control.Monad.Raise
import Data.Aeson                         (FromJSON, ToJSON, FromJSONKey, ToJSONKey, parseJSON, encode, (.=), (.:))
import qualified Data.Aeson               as JSON
import qualified Data.Aeson.Types         as JSON
import qualified Data.Aeson.Encoding      as JSON
import qualified Data.ByteString.Lazy     as BS
import Control.Lens.Aeson

-- === Definition === --

data Initialize = Initialize { initialize :: Applications
                             } deriving (Generic, Eq)

data Applications = Applications { askEmail     :: Bool
                                 , versionTypes :: [Text]
                                 , applications :: [Apps]
                                 } deriving (Generic, Eq)

data Apps = Apps { name     :: Text
                 , versions :: Versions
                 } deriving (Generic, Eq)

data Versions = Versions { developer :: [Version]
                         , nightly   :: [Version]
                         , release   :: [Version]
                         } deriving Eq

data Option = Option { install :: Install} deriving (Generic, Eq)

data Install = Install { application :: Text
                       , version     :: Version
                       , email       :: Maybe Text
                       } deriving (Generic, Eq)

-- makeLenses ''Apps

instance ToJSON Option
instance ToJSON Install
instance ToJSON Initialize
instance ToJSON Applications
instance ToJSON Apps      --  where toEncoding = lensJSONToEncoding; toJSON = lensJSONToJSON

-- Fields prefixed with a dot will be hidden in the Installer.
instance ToJSON Versions where
    toJSON (Versions d n r) =
        JSON.object ["release" .= r, "nightly" .= n, ".developer" .= d]
    toEncoding (Versions d n r) =
        JSON.pairs  ("release" .= r <> "nightly" .= n <> ".developer" .= d)

instance FromJSON Option
instance FromJSON Install
instance FromJSON Initialize   where parseJSON = lensJSONParse
instance FromJSON Applications where parseJSON = lensJSONParse
instance FromJSON Apps         where parseJSON = lensJSONParse

-- Fields prefixed with a dot will be hidden in the installer
instance FromJSON Versions where
    parseJSON = JSON.withObject "Versions" $ \v -> Versions
        <$> v .: ".developer"
        <*> v .: "nightly"
        <*> v .: "release"

resolveAppToInitialize :: (MonadIO m, MonadException SomeException m) => Repo -> Text -> m Apps
resolveAppToInitialize repo name = do
    (devs, nightlies, releases) <- getGroupedVersionsList repo name
    return $ Apps name $ Versions devs nightlies releases

generateInitialJSON :: (MonadIO m, MonadException SomeException m) => Repo -> Bool -> m ()
generateInitialJSON repo userInfoExists = do
    let verTypes = ["release", "nightly", "developer"]
    resolved <- mapM (resolveAppToInitialize repo) (repo ^. apps)
    print $ encode $ Initialize $ Applications (not userInfoExists) verTypes resolved
