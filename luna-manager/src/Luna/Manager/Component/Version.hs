module Luna.Manager.Component.Version where

import Prologue
import Control.Lens.Aeson
import Luna.Manager.Component.Pretty

import           Data.Aeson          (FromJSON, ToJSON, FromJSONKey, ToJSONKey, parseJSON)
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Encoding as JSON
import           Data.Maybe          (isNothing)
import qualified Data.Text           as Text
import Control.Error.Util (hush)

------------------------
-- === Versioning === --
------------------------

-- === Definition === --

data VersionInfo = VersionInfo { _nightlyNumber :: !Word64
                               , _buildNumber   :: !(Maybe Word64)
                               } deriving (Generic, Show, Eq, Ord)

data Version = Version { _major :: !Word64
                       , _minor :: !Word64
                       , _info  :: !(Maybe VersionInfo)
                       } deriving (Generic, Show, Eq, Ord)

makeLenses ''Version
makeLenses ''VersionInfo

isRelease, isNightly, isDev :: Version -> Bool
isRelease   = isNothing . (view info)
isDev       = isJust . preview (info . traverse . buildNumber . traverse)
isNightly v = (not $ isRelease v) && (not $ isDev v)

cShow = convert . show

isBuild, isNightly, isRelease :: Version -> Bool
isBuild   v = case v ^. info of Just (VersionInfo _ Build)   -> True; _ -> False
isNightly v = case v ^. info of Just (VersionInfo _ Nightly) -> True; _ -> False
isRelease v = isNothing $ v ^. info

nextBuild :: Version -> Version
nextBuild = info %~ nextInfo
    where nextInfo i = case i of
            Just (VersionInfo b _) -> Just (VersionInfo (b + 1) Build)
            Nothing                -> Just (VersionInfo       1 Build)

promoteToNightly :: Version -> Either Text Version
promoteToNightly v = case v ^? info . traverse . tag of
    Nothing      -> Left "Cannot promote a release to nightly"
    Just Nightly -> Left "Cannot promote nightly to nightly"
    Just Build   -> Right (v & info . traverse . tag .~ Nightly)

promoteToRelease :: Version -> Either Text Version
promoteToRelease v = case v ^? info . traverse . tag of
    Nothing      -> Left "Cannot promote a release to release"
    Just Build   -> Left "Cannot promote build to release (need nightly first)"
    Just Nightly -> Right (v & info .~ Nothing & minor %~ (+1))


-- === Instances === --

instance Pretty VersionInfo where
    showPretty (VersionInfo nn bn) = cShow nn <> maybe "" (("." <>) . cShow) bn
    readPretty s = case Text.splitOn "." s of
        (nn:bn:_) -> mapLeft convert $ VersionInfo <$> tryReads nn <*> Right (hush (tryReads bn))
        [nn]      -> mapLeft convert $ VersionInfo <$> tryReads nn <*> pure Nothing
        _         -> Left "Incorrect version info format. Expected: <nightly_number>[.<build_number>]"

instance Pretty Version where
    showPretty (Version ma mi info) = intercalate "." (map (convert . show) [ma, mi])
                                    <> maybe "" (("." <>) . showPretty) info
    readPretty t = case Text.splitOn "." t of
        [ma, mi]   -> cerr $ Version <$> tryReads ma <*> tryReads mi <*> pure Nothing
        (ma:mi:nn) -> cerr $ Version <$> tryReads ma <*> tryReads mi <*> Right (hush $ readPretty $ Text.intercalate "." nn)
        _           -> Left "Incorrect version format"
        where cerr = mapLeft convert

-- JSON
instance ToJSON      Version     where toEncoding  = JSON.toEncoding . showPretty; toJSON = JSON.toJSON . showPretty
instance ToJSON      VersionInfo where toEncoding  = JSON.toEncoding . showPretty; toJSON = JSON.toJSON . showPretty
instance FromJSON    Version     where parseJSON   = either (fail . convert) return . readPretty <=< parseJSON
instance FromJSON    VersionInfo where parseJSON   = either (fail . convert) return . readPretty <=< parseJSON
instance FromJSONKey Version     where fromJSONKey = JSON.FromJSONKeyTextParser $ either (fail . convert) return . readPretty
instance ToJSONKey   Version     where
    toJSONKey = JSON.ToJSONKeyText f g
        where f = showPretty
              g = JSON.text . showPretty


instance Default Version where
    def = Version 0 0 (Just $ VersionInfo 0 Build)
