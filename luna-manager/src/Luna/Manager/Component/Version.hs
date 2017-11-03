module Luna.Manager.Component.Version where

import Prologue
import Control.Lens.Aeson
import Luna.Manager.Component.Pretty

import           Data.Aeson          (FromJSON, ToJSON, FromJSONKey, ToJSONKey, parseJSON)
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Encoding as JSON
import qualified Data.Text           as Text
import Control.Error.Util (hush)

------------------------
-- === Versioning === --
------------------------

-- === Definition === --

data VersionInfo = Nightly
                 | Build
                 deriving (Generic, Show, Eq, Ord)

data Version = Version { _major :: !Word64
                       , _minor :: !Word64
                       , _build :: !Word64
                       , _info  :: !(Maybe VersionInfo)
                       } deriving (Generic, Show, Eq, Ord)

makeLenses ''Version
makeLenses ''VersionInfo

nextVersion :: Version -> Version
nextVersion = (build %~ (+1)) . (info .~ (Just Build))

-- === Instances === --

instance Pretty VersionInfo where
    showPretty Nightly = "nightly"
    showPretty Build   = "build"
    readPretty s = case s of
        "nightly" -> Right Nightly
        "build"   -> Right Build
        _ -> Left "Incorrect version info format. Expected: \"build\"|\"nightly\""
        where tryConvert str = mapLeft (const "Conversion error") (tryReads @String str)

instance Pretty Version where
    showPretty (Version major minor build info) = intercalate "." (map (convert . show) [major, minor, build])
                                                <> maybe "" (("-" <>) . showPretty) info
    readPretty t = case Text.splitOn "." =<< Text.splitOn "-" t of
        [ma, mi, b, x] -> cerr $ Version <$> tryReads ma <*> tryReads mi <*> tryReads b <*> Right (hush (readPretty x))
        [ma, mi, b]    -> cerr $ Version <$> tryReads ma <*> tryReads mi <*> tryReads b <*> pure Nothing
        _              -> Left "Incorrect version format"
        where cerr = mapLeft convert

-- JSON
instance ToJSON      Version     where toEncoding  = JSON.toEncoding . showPretty; toJSON = JSON.toJSON . showPretty
instance ToJSON      VersionInfo where toEncoding  = lensJSONToEncoding; toJSON = lensJSONToJSON
instance FromJSON    Version     where parseJSON   = either (fail . convert) return . readPretty <=< parseJSON
instance FromJSON    VersionInfo where parseJSON   = lensJSONParse
instance FromJSONKey Version     where fromJSONKey = JSON.FromJSONKeyTextParser $ either (fail . convert) return . readPretty
instance ToJSONKey   Version     where
    toJSONKey = JSON.ToJSONKeyText f g
        where f = showPretty
              g = JSON.text . showPretty
