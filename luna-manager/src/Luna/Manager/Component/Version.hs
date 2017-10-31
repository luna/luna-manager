module Luna.Manager.Component.Version where

import Prologue
import Control.Lens.Aeson
import Luna.Manager.Component.Pretty

import           Data.Aeson          (FromJSON, ToJSON, FromJSONKey, ToJSONKey, parseJSON)
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Encoding as JSON
import qualified Data.Time.Clock     as Time
import qualified Data.Time.Format    as Time
import qualified Data.Text           as Text
import Control.Error.Util (hush)

------------------------
-- === Versioning === --
------------------------

-- === Definition === --

data VersionTag = Alpha
                | Beta
                | RC !Word64
                deriving (Generic, Show, Eq, Ord)

data VersionInfo = Nightly !Word64
                 | Build   !Word64
                 deriving (Generic, Show, Eq, Ord)

data Version = Version { _major :: !Word64
                       , _minor :: !Word64
                       , _build :: !Word64
                       , _tag   :: !(Maybe VersionTag)
                       , _info  :: !(Maybe VersionInfo)
                       } deriving (Generic, Show, Eq, Ord)

makeLenses ''Version
makeLenses ''VersionInfo
makeLenses ''VersionTag

nextVersion :: MonadIO m => Version -> m Version
nextVersion v = do
    let format = Time.formatTime Time.defaultTimeLocale "%Y%m%d"
    timestamp <- liftIO $ format <$> Time.getCurrentTime
    return (v & info .~ (Just . Build . read $ timestamp))

baseVersion :: Version -> Version
baseVersion = (tag .~ Nothing) . (info .~ Nothing)

-- === Instances === --

instance Pretty VersionTag where
    showPretty = \case
        Alpha   -> "alpha"
        Beta    -> "beta"
        RC i    -> "rc" <> convert (show i)
    readPretty = \case
        "alpha"   -> Right Alpha
        "beta"    -> Right Beta
        s         -> case Text.take 2 s of
            "rc" -> RC <$> mapLeft (const "Conversion error") (tryReads @String $ Text.drop 2 s)
            _    -> Left "Incorrect version tag format"

instance Pretty VersionInfo where
    showPretty (Nightly i) = "nightly" <> convert (show i)
    showPretty (Build   i) = "build"   <> convert (show i)
    readPretty s = case Text.take 5 s of
        "night" -> Nightly <$> mapLeft (const "Conversion error") (tryReads @String $ Text.drop 7 s)
        "build" -> Build   <$> mapLeft (const "Conversion error") (tryReads @String $ Text.drop 5 s)
        _ -> Left "Incorrect version tag format"

instance Pretty Version where
    showPretty (Version major minor build tag nightly) = intercalate "." (map (convert . show) [major, minor, build])
                                              <> maybe "" (("." <>) . showPretty) tag <> maybe "" (("." <>) . showPretty) nightly
    readPretty t = case Text.splitOn "." t of
        [ma, mi, b, t, i] -> cerr $ Version <$> tryReads ma <*> tryReads mi <*> tryReads b <*> bimap convert Just (readPretty t) <*> bimap convert Just (readPretty i)
        [ma, mi, b, x]    -> cerr $ Version <$> tryReads ma <*> tryReads mi <*> tryReads b <*> Right (hush (readPretty x)) <*> Right (hush (readPretty x))
        [ma, mi, b]       -> cerr $ Version <$> tryReads ma <*> tryReads mi <*> tryReads b <*> pure Nothing <*> pure Nothing
        _                 -> Left "Incorrect version format"
        where cerr = mapLeft convert

-- JSON
instance ToJSON      Version     where toEncoding  = JSON.toEncoding . showPretty; toJSON = JSON.toJSON . showPretty
instance ToJSON      VersionInfo where toEncoding  = lensJSONToEncoding; toJSON = lensJSONToJSON
instance ToJSON      VersionTag  where toEncoding  = lensJSONToEncoding; toJSON = lensJSONToJSON
instance FromJSON    Version     where parseJSON   = either (fail . convert) return . readPretty <=< parseJSON
instance FromJSON    VersionInfo where parseJSON   = lensJSONParse
instance FromJSON    VersionTag  where parseJSON   = lensJSONParse
instance FromJSONKey Version     where fromJSONKey = JSON.FromJSONKeyTextParser $ either (fail . convert) return . readPretty
instance ToJSONKey   Version     where
    toJSONKey = JSON.ToJSONKeyText f g
        where f = showPretty
              g = JSON.text . showPretty
