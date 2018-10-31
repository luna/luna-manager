{-# LANGUAGE CPP #-}

module Main where

import Control.Monad.Raise
import Luna.Manager.Clean
import Luna.Manager.Command              (chooseCommand)
import Luna.Manager.Command.Options      (evalOptionsParserT, parseOptions)
import Luna.Manager.Component.Version.TH (getVersion)
import Luna.Manager.System.Env           (EnvConfig, getTmpPath)
import Luna.Manager.System.Host          (evalDefHostConfig)
import Prologue                          hiding (FilePath)


import           Control.Concurrent          (myThreadId)
import qualified Control.Exception.Safe      as Exception
import           Control.Monad.State.Layered
import qualified Luna.Manager.Shell.Shelly   as Shelly
import           System.IO                   (hPutStrLn, stderr)


main :: IO ()
main = run

run :: (MonadIO m, MonadException SomeException m, MonadMask m) => m ()
run = Shelly.shelly $ do
    options  <- parseOptions
    tmp      <- evalDefHostConfig @EnvConfig $ evalStateT getTmpPath options
    threadId <- liftIO myThreadId
    liftIO $ handleSignal threadId
    Exception.handleAny handleTopLvlError $ evalOptionsParserT chooseCommand `Exception.finally` (cleanUp tmp)

handleTopLvlError :: (MonadIO m, MonadMask m, Shelly.MonadSh m) => SomeException -> m ()
handleTopLvlError e = do
    liftIO $ hPutStrLn stderr $ (displayException e) <> "\nManager version: " <> $(getVersion)
    Shelly.quietExit 1
