-- A module for tracking user behaviour using the Mixpanel API
-- See https://mixpanel.com/help/reference/http for reference.

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Luna.Manager.Component.Analytics (
    MPUserData(..),
    mpRegisterUser,
    mpTrackEvent,
    userInfoExists
) where

import           Prologue                      hiding ((.=), FilePath)

import           Control.Lens.Aeson            (lensJSONToJSON, lensJSONToEncoding)
import           Control.Monad.State.Layered   as SL
import           Control.Monad.Trans.Resource  (MonadBaseControl)
import           Data.Aeson                    (FromJSON, ToJSON, toEncoding, toJSON, (.=))
import qualified Data.Aeson                    as JSON
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as BS
import           Data.ByteString.Lazy          (toStrict)
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Base64        as Base64
import           Data.Maybe                    (maybeToList)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Data.UUID                     (UUID)
import qualified Data.UUID                     as UUID
import qualified Data.UUID.V1                  as UUID
import qualified Data.UUID.V4                  as UUID
import           Filesystem.Path.CurrentOS     (FilePath)
import qualified Filesystem.Path.CurrentOS     as FilePath
import qualified Network.HTTP.Simple           as HTTP

import           Luna.Manager.Command.Options  (Options)
import           Luna.Manager.Component.Pretty (showPretty)
import qualified Luna.Manager.Logger           as Logger
import           Luna.Manager.Shell.Shelly     (MonadSh, MonadShControl)
import qualified Luna.Manager.Shell.Shelly     as Shelly
import           Luna.Manager.System.Host
import           Luna.Manager.System.Env       (EnvConfig)

default(Text)

----------------------------------------------------------------------------
-- JSON-serializable data types conforming to Mixpanel's API requirements --
----------------------------------------------------------------------------

data MPEvent = MPEvent
             { _event      :: Text         -- A name for the event
             , _properties :: MPEventProps -- Event properties used to filter and segment events in MP
             } deriving (Show, Eq, Generic)

data MPEventProps = MPEventProps
                  { _token       :: Text    -- MP token associated with the project
                  , _distinct_id :: Text    -- Used to identify users who triggered the event. Optional within MP, mandatory here.
                  } deriving (Show, Eq, Generic)

data MPUserUpdate = MPUserUpdate
                  { _tkn :: Text       -- MP token associated with the project
                  , _did :: Text       -- Same as distinct_id. Used to identify users who triggered the event. Optional within MP, mandatory here.
                  , _set :: MPUserData -- Data to set in the user profile
                  } deriving (Show, Eq)

data MPUserData = MPUserData
                { _userInfoUUID   :: UUID
                , _userInfoEmail  :: Text
                , _userInfoOsType :: System
                , _userInfoOsDist :: Text
                , _userInfoOsVer  :: Text
                , _userInfoArch   :: SysArch
                } deriving (Show, Eq, Generic)

makeLenses ''MPEvent
makeLenses ''MPEventProps
makeLenses ''MPUserUpdate
makeLenses ''MPUserData

instance ToJSON MPEvent where
    toEncoding = lensJSONToEncoding
    toJSON     = lensJSONToJSON

instance ToJSON MPEventProps where
    toEncoding = lensJSONToEncoding
    toJSON     = lensJSONToJSON

instance ToJSON MPUserData where
    toEncoding = lensJSONToEncoding
    toJSON     = lensJSONToJSON

instance FromJSON MPUserData

instance ToJSON MPUserUpdate where
    toJSON     (MPUserUpdate t d s) = JSON.object ["$token" .= t,   "$distinct_id" .= d,   "$set" .= s]
    toEncoding (MPUserUpdate t d s) = JSON.pairs  ("$token" .= t <> "$distinct_id" .= d <> "$set" .= s)

instance Default MPUserData where
    def = MPUserData UUID.nil "" currentHost "" "" currentArch


-----------------------------------------------
-- User machine discovery and identification --
-----------------------------------------------

newUuid :: MonadIO m => m UUID
newUuid = liftIO $ do
    v1 <- UUID.nextUUID
    case v1 of
        Just uuid -> return uuid
        Nothing   -> UUID.nextRandom

osVersion :: (MonadShControl m, MonadSh m) => m Text
osVersion = Shelly.silently $ case currentHost of
        Windows ->   Text.strip . (!! 1) . Text.splitOn ":" . head
                 .   filter ("OS Name" `Text.isPrefixOf`) . Text.lines
                 <$> Shelly.cmd "systeminfo"
        Linux   -> Shelly.cmd "awk" "'/^VERSION=/'" "/etc/*-release" >>=
                   Shelly.cmd "awk" "-F'='" "'{ print tolower($2) }'"
        Darwin  -> Text.strip <$> Shelly.cmd "sw_vers" "-productVersion"

osDistro :: (MonadShControl m, MonadSh m) => m Text
osDistro = Shelly.silently $ case currentHost of
        Windows -> return "N/A"
        Linux   -> Shelly.cmd "awk" "'/^ID=/'" "/etc/*-release" >>=
                   Shelly.cmd "awk" "-F'='" "'{ print tolower($2) }'"
        Darwin  -> return "N/A"

-- Gets basic info about the operating system the installer is running on.
userInfo :: (MonadIO m, MonadBaseControl IO m, MonadSh m, MonadShControl m) =>
             Text -> m MPUserData
userInfo email = do
    let safeGet item = Shelly.catchany item (const $ return "unknown")
    uuid <- newUuid
    ver  <- safeGet osVersion
    dist <- safeGet osDistro
    return $ MPUserData { _userInfoUUID   = uuid
                        , _userInfoEmail  = email
                        , _userInfoOsType = currentHost
                        , _userInfoOsDist = dist
                        , _userInfoOsVer  = ver
                        , _userInfoArch   = currentArch
                        }

-- Checks whether we already have the right user info saved in ~/.luna/user_info.json
userInfoExists :: (MonadSh m, MonadIO m) => FilePath -> m Bool
userInfoExists userInfoPath = do
    fileExists <- Shelly.test_f userInfoPath
    if not fileExists then return False
    else do
        bytes <- liftIO $ BSL.readFile $ FilePath.encodeString userInfoPath
        let userInfoM = JSON.decode bytes :: Maybe MPUserData
            userInfo  = fromMaybe def userInfoM
        return $ userInfo ^. userInfoUUID == UUID.nil

-- Saves the email, along with some OS info, to a file user_info.json.
processUserEmail :: (MonadSh m, MonadShControl m, MonadIO m, MonadBaseControl IO m) =>
                     FilePath -> Text -> m MPUserData
processUserEmail userInfoPath email = do
    info  <- userInfo email
    Shelly.mkdir_p $ FilePath.parent userInfoPath
    Shelly.touchfile userInfoPath
    liftIO $ BS.writeFile (FilePath.encodeString userInfoPath) (toStrict $ JSON.encode info)
    return info


-----------------------------------------------------------------
-- Convenience methods for serializing and sending the events  --
-----------------------------------------------------------------

projectToken :: Text
projectToken = "0d906436719b047c86b7fee8ae550601"

eventEndpoint :: String
eventEndpoint = "http://api.mixpanel.com/track/"

userUpdateEndpoint :: String
userUpdateEndpoint = "http://api.mixpanel.com/engage/"

serialize :: ToJSON s => s -> ByteString
serialize = Base64.encode . toStrict . JSON.encode

sendMpRequest :: (ToJSON s, MonadIO m, MonadThrow m) => s -> m ()
sendMpRequest s = do
    let payload = serialize s
        param   = "data" :: ByteString
    request <- HTTP.setRequestQueryString [(param, Just payload)] <$>
               HTTP.parseRequest userUpdateEndpoint
    liftIO $ void $ HTTP.httpNoBody request

mpRegisterUser :: (MonadIO m, MonadSetter MPUserData m, MonadThrow m,
                   MonadShControl m, MonadSh m, MonadBaseControl IO m) =>
                   FilePath -> Text -> m ()
mpRegisterUser userInfoPath email = Shelly.unlessM (userInfoExists userInfoPath) $ do
    userData <- processUserEmail userInfoPath email
    let uuid   = UUID.toText $ userData ^. userInfoUUID
        mpData = MPUserUpdate { _tkn = projectToken
                              , _did = uuid
                              , _set = userData
                              }
    put @MPUserData userData
    sendMpRequest mpData

mpTrackEvent :: (MonadIO m, MonadGetters '[MPUserData, Options, EnvConfig] m,
                 MonadThrow m, MonadSh m, MonadShControl m) => Text -> m ()
mpTrackEvent eventName = do
    uuid <- gets @MPUserData userInfoUUID
    let mpProps = MPEventProps { _token       = projectToken
                               , _distinct_id = UUID.toText uuid
                               }
        mpData  = MPEvent  { _event      = eventName
                           , _properties = mpProps
                           }
    sendMpRequest mpData
