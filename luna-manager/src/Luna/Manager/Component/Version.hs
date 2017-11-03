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

data VersionTag = Nightly
                 | Build
                 deriving (Generic, Show, Eq, Ord)

data VersionInfo = VersionInfo { _build :: !Word64
                               , _tag   :: VersionTag
                               } deriving (Generic, Show, Eq, Ord)

data Version = Version { _major :: !Word64
                       , _minor :: !Word64
                       , _info  :: !(Maybe VersionInfo)
                       } deriving (Generic, Show, Eq, Ord)

makeLenses ''Version
makeLenses ''VersionInfo

cShow = convert . show

-- === Instances === --

instance Pretty VersionTag where
    showPretty Nightly = "nightly"
    showPretty Build   = "build"
    readPretty s = case s of
        "nightly" -> Right Nightly
        "build"   -> Right Build
        _ -> Left "Incorrect version tag format. Expected: \"build\"|\"nightly\""

instance Pretty VersionInfo where
    showPretty (VersionInfo b t) = cShow b <> "-" <> showPretty t
    readPretty s = case Text.splitOn "-" s of
        [b, t] -> mapLeft convert $ VersionInfo <$> tryReads b <*> mapLeft convert (readPretty t)
        _      -> Left "Incorrect version info format. Expected: <build_no>-<version_tag>"

instance Pretty Version where
    showPretty (Version ma mi info) = intercalate "." (map (convert . show) [ma, mi])
                                    <> maybe "" (("." <>) . showPretty) info
    readPretty t = case Text.splitOn "." t of
        [ma, mi, i] -> cerr $ Version <$> tryReads ma <*> tryReads mi <*> Right (hush (readPretty i))
        [ma, mi]    -> cerr $ Version <$> tryReads ma <*> tryReads mi <*> pure Nothing
        _           -> Left "Incorrect version format"
        where cerr = mapLeft convert

-- JSON
instance ToJSON      Version     where toEncoding  = JSON.toEncoding . showPretty; toJSON = JSON.toJSON . showPretty
instance ToJSON      VersionInfo where toEncoding  = JSON.toEncoding . showPretty; toJSON = JSON.toJSON . showPretty
instance ToJSON      VersionTag  where toEncoding  = JSON.toEncoding . showPretty; toJSON = JSON.toJSON . showPretty
instance FromJSON    Version     where parseJSON   = either (fail . convert) return . readPretty <=< parseJSON
instance FromJSON    VersionInfo where parseJSON   = either (fail . convert) return . readPretty <=< parseJSON
instance FromJSON    VersionTag  where parseJSON   = either (fail . convert) return . readPretty <=< parseJSON
instance FromJSONKey Version     where fromJSONKey = JSON.FromJSONKeyTextParser $ either (fail . convert) return . readPretty
instance ToJSONKey   Version     where
    toJSONKey = JSON.ToJSONKeyText f g
        where f = showPretty
              g = JSON.text . showPretty
