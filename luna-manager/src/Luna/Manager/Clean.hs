{-# LANGUAGE CPP #-}

module Luna.Manager.Clean where

import Prologue hiding (FilePath)

import           Control.Concurrent        (ThreadId, killThread)
import           Filesystem.Path.CurrentOS (FilePath, encodeString)
import           GHC.ConsoleHandler
import           System.Directory          (removeDirectoryRecursive)
import qualified System.Signal             as Signal


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
