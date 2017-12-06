-- A module for tracking user behaviour using the Mixpanel API
-- See https://mixpanel.com/help/reference/http for reference.

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Luna.Manager.Component.Analytics (
    MPUserData(..),
    mpRegisterUser,
    mpTrackEvent
) where

import           Prologue                     hiding ((.=))

import           Control.Lens.Aeson           (lensJSONToJSON, lensJSONToEncoding)
import           Control.Monad.State.Layered  as SL
import           Data.Aeson                   (ToJSON, toEncoding, toJSON, (.=))
import qualified Data.Aeson                   as JSON
import           Data.ByteString              (ByteString)
import           Data.ByteString.Lazy         (toStrict)
import qualified Data.ByteString.Base64       as Base64
import           Data.Maybe                   (maybeToList)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Network.HTTP.Simple          as HTTP

import           Luna.Manager.Command.Options (Options)
import qualified Luna.Manager.Logger          as Logger
import           Luna.Manager.Shell.Shelly    (MonadSh, MonadShControl)
import           Luna.Manager.System.Env      (EnvConfig)

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
                { _userInfoUUID  :: Text
                , _userInfoEmail :: Text
                , _userInfoOs    :: Text
                , _userInfoOsVer :: Text
                , _userInfoArch  :: Text
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

instance ToJSON MPUserUpdate where
    toJSON     (MPUserUpdate t d s) = JSON.object ["$token" .= t,   "$distinct_id" .= d,   "$set" .= s]
    toEncoding (MPUserUpdate t d s) = JSON.pairs  ("$token" .= t <> "$distinct_id" .= d <> "$set" .= s)

instance Default MPUserData where
    def = MPUserData "" "" "" "" ""

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

mpRegisterUser :: (MonadIO m, MonadSetter MPUserData m, MonadThrow m) =>
                   MPUserData -> m ()
mpRegisterUser userData = do
    let mpData = MPUserUpdate { _tkn = projectToken
                              , _did = userData ^. userInfoUUID
                              , _set = userData
                              }
    put @MPUserData userData
    sendMpRequest mpData

mpTrackEvent :: (MonadIO m, MonadGetters '[MPUserData, Options, EnvConfig] m,
                 MonadThrow m, MonadSh m, MonadShControl m) => Text -> m ()
mpTrackEvent eventName = do
    uuid <- gets @MPUserData userInfoUUID
    when (Text.null uuid) $ Logger.warning "User not registered with Mixpanel."
    let mpProps = MPEventProps { _token       = projectToken
                               , _distinct_id = uuid
                               }
        mpData  = MPEvent  { _event      = eventName
                           , _properties = mpProps
                           }
    sendMpRequest mpData
