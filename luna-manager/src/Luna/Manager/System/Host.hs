{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Manager.System.Host where

import Prologue
import Luna.Manager.Component.Pretty
import Control.Lens.Aeson

import qualified Control.Monad.State.Layered as State
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Encoding as JSON
import qualified Data.Text           as Text
import qualified Control.Lens.Aeson  as LensJSON
import qualified Type.Known          as Type

import           Data.Aeson          (FromJSON, ToJSON, FromJSONKey, ToJSONKey)


-------------------
-- === Hosts === --
-------------------

-- === Definition === --

data System = Linux
            | Darwin
            | Windows
            deriving (Generic, Show, Read, Eq, Ord)

data SysArch = X32 | X64              deriving (Generic, Show, Read, Eq, Ord)
data SysDesc = SysDesc System SysArch deriving (Generic, Show, Eq, Ord)


-- === System discovery === --

currentHost :: System


#ifdef linux_HOST_OS
type CurrentHost = 'Linux
currentHost      =  Linux
#elif darwin_HOST_OS
type CurrentHost = 'Darwin
currentHost      =  Darwin
#elif mingw32_HOST_OS
type CurrentHost = 'Windows
currentHost      =  Windows
#else
Running on unsupported system.
#endif


-- === Arch discovery === --

currentArch :: SysArch

#ifdef i386_HOST_ARCH
type CurrentArch = 'X32
currentArch      =  X32
#elif x86_64_HOST_ARCH
type CurrentArch = 'X64
currentArch      =  X64
#else
Running on unsupported system architecture.
#endif


-- === Utils === --

currentSysDesc :: SysDesc
currentSysDesc = SysDesc currentHost currentArch

instance Type.Known 'Linux   where val = Linux
instance Type.Known 'Darwin  where val = Darwin
instance Type.Known 'Windows where val = Windows

instance Type.Known 'X32     where val = X32
instance Type.Known 'X64     where val = X64


-- === Instances === --

-- JSON
instance ToJSON   System  where toEncoding = LensJSON.toEncodingDropUnary; toJSON = LensJSON.toJSONDropUnary
instance ToJSON   SysArch where toEncoding = LensJSON.toEncodingDropUnary; toJSON = LensJSON.toJSONDropUnary
instance ToJSON   SysDesc where toEncoding = LensJSON.toEncodingDropUnary; toJSON = LensJSON.toJSONDropUnary
instance FromJSON System  where parseJSON  = LensJSON.parseDropUnary
instance FromJSON SysArch where parseJSON  = LensJSON.parseDropUnary
instance FromJSON SysDesc where parseJSON  = LensJSON.parseDropUnary
instance FromJSONKey SysDesc where
    fromJSONKey = JSON.FromJSONKeyTextParser $ either (fail . convert) return . readPretty
instance ToJSONKey   SysDesc where
    toJSONKey = JSON.ToJSONKeyText f g
        where f = showPretty
              g = JSON.text . showPretty

-- Show
instance Pretty SysDesc where
    showPretty (SysDesc s a) = showPretty s <> "." <> showPretty a
    readPretty t = case Text.splitOn "." t of
        [s,a] -> mapLeft (const "Conversion error") $ SysDesc <$> readPretty s <*> readPretty a
        _     -> Left "Incorrect system architecture format"

instance Pretty System  where
    showPretty = Text.toLower . convert . show
    readPretty = mapLeft (const "Conversion error") . tryReads . Text.toTitle

instance Pretty SysArch where
    showPretty = Text.toLower . convert . show
    readPretty = mapLeft (const "Conversion error") . tryReads . Text.toTitle



-------------------------------------------
-- === Host dependend configurations === --
-------------------------------------------

-- === Definition === --

class Monad m => MonadHostConfig cfg (system :: System) (arch :: SysArch) m where
    defaultHostConfig :: m cfg

-- === Utils === --

defaultHostConfigFor :: forall system arch cfg m. MonadHostConfig cfg system arch m => m cfg
defaultHostConfigFor = defaultHostConfig @cfg @system @arch

type MonadHostConfig' cfg = MonadHostConfig cfg CurrentHost CurrentArch
defHostConfig :: MonadHostConfig' cfg m => m cfg
defHostConfig = defaultHostConfigFor @CurrentHost @CurrentArch

evalDefHostConfig :: forall s m a. MonadHostConfig' s m => State.StateT s m a -> m a
evalDefHostConfig p = State.evalT @s p =<< defHostConfig


-- === Multiple configs evaluator ===

class MultiConfigRunner (cfgs :: [*]) m where
    evalDefHostConfigs :: forall a. State.StatesT cfgs m a -> m a

instance (MultiConfigRunner ss m, MonadHostConfig' s (State.StatesT ss m))
      => MultiConfigRunner (s ': ss) m where evalDefHostConfigs = evalDefHostConfigs @ss . evalDefHostConfig
instance MultiConfigRunner '[]       m where evalDefHostConfigs = id
