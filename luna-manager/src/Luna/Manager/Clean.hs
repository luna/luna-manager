{-# LANGUAGE CPP #-}

module Luna.Manager.Clean where

import Prologue hiding (FilePath)

import Luna.Manager.Command.Options
import Luna.Manager.System.Env (getTmpPath, EnvConfig)
import Luna.Manager.System.Host (evalDefHostConfigs)

import Control.Concurrent (threadDelay, ThreadId, killThread, myThreadId)
import Control.Monad.Raise
import Control.Monad.State.Layered
import Filesystem.Path.CurrentOS (encodeString, decodeString, FilePath)
import GHC.ConsoleHandler
import qualified System.Signal as Signal
import System.Directory (removeDirectoryRecursive)
import Luna.Manager.Shell.Shelly (MonadSh)


cleanUp :: MonadIO m => FilePath -> m ()
cleanUp tmp = liftIO $ removeDirectoryRecursive $ encodeString tmp

#ifdef mingw32_HOST_OS
termHandler :: ThreadId -> Handler
termHandler threadId  = Catch $ \_ -> killThread threadId
handleSignal :: ThreadId -> IO ()
handleSignal threadId = void $ installHandler $ termHandler threadId
#else
termHandler :: ThreadId -> Signal.Signal -> IO ()
termHandler threadId s = killThread threadId
handleSignal :: ThreadId -> IO ()
handleSignal threadId = do
    Signal.installHandler Signal.sigTERM $ termHandler threadId
    Signal.installHandler Signal.sigINT  $ termHandler threadId
#endif
