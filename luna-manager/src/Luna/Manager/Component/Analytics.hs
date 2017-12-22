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

import           Control.Lens.Aeson            (lensJSONToJSON, lensJSONToEncoding, lensJSONParse)
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
import qualified System.IO                     as SIO

import           Luna.Manager.Command.Options  (Options)
import           Luna.Manager.Component.Pretty (showPretty)
import qualified Luna.Manager.Logger           as Logger
import           Luna.Manager.Shell.Shelly     (MonadSh, MonadShControl)
import qualified Luna.Manager.Shell.Shelly     as Shelly
import           Luna.Manager.System.Host
import           Luna.Manager.System.Env       (EnvConfig)
import           Luna.Manager.System.Path      (expand)

default(Text)

----------------------------------------------------------------------------
-- JSON-serializable data types conforming to Mixpanel's API requirements --
----------------------------------------------------------------------------

data MPEvent = MPEvent
             { _event      :: Text          -- A name for the event
             , _properties :: MPEventProps  -- Event properties used to filter and segment events in MP
             } deriving (Show, Eq, Generic)

data MPEventProps = MPEventProps
                  { _token       :: Text    -- MP token associated with the project
                  , _distinct_id :: Text    -- Used to identify users who triggered the event. Optional within MP, mandatory here.
                  } deriving (Show, Eq, Generic)

data MPUserUpdate = MPUserUpdate
                  { _tkn :: Text            -- MP token associated with the project
                  , _did :: Text            -- Same as distinct_id. Used to identify users who triggered the event. Optional within MP, mandatory here.
                  , _set :: MPUserData      -- Data to set in the user profile
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

instance FromJSON MPUserData where
    parseJSON = lensJSONParse

instance ToJSON MPUserUpdate where
    toJSON     (MPUserUpdate t d s) = JSON.object ["$token" .= t,   "$distinct_id" .= d,   "$set" .= s]
    toEncoding (MPUserUpdate t d s) = JSON.pairs  ("$token" .= t <> "$distinct_id" .= d <> "$set" .= s)

instance Default MPUserData where
    def = MPUserData UUID.nil "" currentHost "" "" currentArch


-----------------------------------------------
-- User machine discovery and identification --
-----------------------------------------------

-- Generate a fresh UUID, using V1 by default.
newUuid :: MonadIO m => m UUID
newUuid = liftIO $ do
    v1 <- UUID.nextUUID
    case v1 of
        Just uuid -> return uuid
        Nothing   -> UUID.nextRandom

-- Transforms sth like 'VARNAME="some_text"' into 'some_text'.
stripVarName :: Text -> Text -> Text
stripVarName varName txt = fromMaybe "unknown" stripped
    where stripped = Text.filter (/= '"') <$> (Text.stripPrefix varName $ Text.strip txt)

-- (linux) lookup a var in /etc/*-release files
lookupSysVar :: (MonadShControl m, MonadSh m) => Text -> m Text
lookupSysVar varName = do
    let awkClause = "'/^" <> varName <> "=/'"
    line <- Shelly.escaping False $ Shelly.cmd "awk" awkClause "/etc/*-release"
    return $ stripVarName (varName <> "=") line

osVersion :: (MonadShControl m, MonadSh m) => m Text
osVersion = do
    Shelly.silently $ case currentHost of
        Windows ->   Text.strip . (!! 1) . Text.splitOn ":" . head
                 .   filter ("OS Name" `Text.isPrefixOf`) . Text.lines
                 <$> Shelly.cmd "systeminfo"
        Linux   -> lookupSysVar "VERSION"
        Darwin  -> Text.strip <$> Shelly.cmd "sw_vers" "-productVersion"

osDistro :: (MonadShControl m, MonadSh m) => m Text
osDistro = Shelly.silently $ case currentHost of
        Windows -> return "N/A"
        Linux   -> lookupSysVar "NAME"
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

-- Write to a file, ensuring that the handle is flushed afterwards.
strictWrite :: ToJSON s => FilePath -> s -> IO ()
strictWrite filePath s = do
    let serialized = toStrict $ JSON.encode s
        path       = FilePath.encodeString filePath
    SIO.withFile path SIO.WriteMode $ \h -> do
        BS.hPut h serialized
        SIO.hFlush h
    return ()

-- Checks whether we already have the right user info saved in ~/.luna/user_info.json
userInfoExists :: (MonadSh m, MonadIO m) => FilePath -> m Bool
userInfoExists userInfoPath = do
    path <- expand userInfoPath
    fileExists <- Shelly.test_f path
    if not fileExists then return False
    else do
        bytes <- liftIO $ BSL.readFile $ FilePath.encodeString path
        let userInfoM = JSON.decode bytes :: Maybe MPUserData
            userInfo  = fromMaybe def userInfoM
        return . not . UUID.null $ userInfo ^. userInfoUUID

-- Saves the email, along with some OS info, to a file user_info.json.
processUserEmail :: (MonadSh m, MonadShControl m, MonadIO m, MonadBaseControl IO m) =>
                     FilePath -> Text -> m MPUserData
processUserEmail userInfoPath email = do
    info  <- userInfo email
    Shelly.mkdir_p $ FilePath.parent userInfoPath
    Shelly.touchfile userInfoPath
    liftIO $ strictWrite userInfoPath info
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

-- Generic wrapper around Mixpanel requests.
sendMpRequest :: (ToJSON s, MonadIO m, MonadThrow m) => String -> s -> m ()
sendMpRequest endpoint s = do
    let payload = serialize s
    request <- HTTP.setRequestQueryString [("data", Just payload)] <$>
               HTTP.parseRequest userUpdateEndpoint
    liftIO $ void $ HTTP.httpNoBody request

-- Register a new user within Mixpanel.
mpRegisterUser :: (MonadIO m, MonadSetter MPUserData m, MonadThrow m,
                   MonadShControl m, MonadSh m, MonadBaseControl IO m) =>
                   FilePath -> Text -> m ()
mpRegisterUser userInfoPath email = Shelly.unlessM (userInfoExists userInfoPath) $ do
    path     <- expand userInfoPath
    userData <- processUserEmail path email
    let uuid   = UUID.toText $ userData ^. userInfoUUID
        mpData = MPUserUpdate { _tkn = projectToken
                              , _did = uuid
                              , _set = userData
                              }
    put @MPUserData userData
    sendMpRequest userUpdateEndpoint mpData
    return ()

-- Send a single event to Mixpanel.
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
    sendMpRequest eventEndpoint mpData
