{-# LANGUAGE ScopedTypeVariables #-}
module Luna.Manager.Shell.Shelly (module Luna.Manager.Shell.Shelly, module X) where

import Shelly.Lifted as X hiding (mv, rm_rf, run, run_)

import Prologue hiding (FilePath)

import qualified Control.Exception.Safe      as Exception
import qualified Control.Monad.State.Layered as State
import qualified Control.Monad.State.Strict  as S
import qualified Luna.Manager.Logger         as Logger
import qualified Shelly.Lifted               as Sh
import qualified System.Process              as Process
import qualified System.Process.Typed        as TypedProcess

import Control.Concurrent      (threadDelay)
import Control.Monad.Exception (MonadException, throw)

import Filesystem.Path.CurrentOS    (FilePath, encodeString)
import Luna.Manager.Command.Options
import Luna.Manager.System.Host


deriving instance MonadSh   m => MonadSh (State.StateT s m)
instance          Exception e => MonadException e Sh.Sh where throw = Exception.throwM

-- Maybe we could simplify it in GHC 8.2 ?
instance MonadShControl m => MonadShControl (State.StateT s m) where
    newtype ShM (State.StateT s m) a = StateTShM { fromShM :: ShM (S.StateT s m) a }
    restoreSh shm = State.StateT $ restoreSh (fromShM shm)
    liftShWith (f :: ((forall x. State.StateT s m x -> Sh (ShM (State.StateT s m) x)) -> Sh a)) = State.StateT $ liftShWith f' where
        f' :: (forall x. S.StateT s m x -> Sh (ShM (S.StateT s m) x)) -> Sh a
        f' h = f h' where
            h' :: (forall x. State.StateT s m x -> Sh (ShM (State.StateT s m) x))
            h' s = fmap StateTShM $ h (unwrap s)


mv :: (Logger.LoggerMonad m, MonadIO m, MonadSh m, MonadCatch m) => FilePath -> FilePath -> m ()
mv src dst = case currentHost of
    Linux   -> cmd "mv" src dst
    Darwin  -> cmd "mv" src dst
    Windows -> do
        Logger.log "Shelly.mv"
        Logger.logObject "src" src
        Logger.logObject "dst" dst
        Sh.mv src dst

runCommand :: MonadIO m => String -> FilePath -> m ()
runCommand cmd path = liftIO $ TypedProcess.runProcess_
                             $ TypedProcess.shell $ cmd <> quotedPath
    where quotedPath = "\"" <> encodeString path <> "\""

runShellCmd :: MonadIO m => Text -> m ()
runShellCmd cmd = liftIO $ TypedProcess.runProcess_
                         $ TypedProcess.shell $ convert cmd
    
runProcess :: (Logger.LoggerMonad m, MonadIO m) => FilePath -> [Text] -> m ()
runProcess path args = do
    let pathStr = encodeString path
        argsStr = map convert args
    (ec, out, err) <- liftIO $ Process.readProcessWithExitCode pathStr argsStr ""
    Logger.log $ convert $ show ec
    Logger.log $ convert out
    Logger.log $ convert err

runRawSystem :: (Logger.LoggerMonad m, MonadIO m) => FilePath -> [Text] -> m ()
runRawSystem path args = do
    let pathStr = encodeString path
        argsStr = map convert args
    ec <- liftIO $ Process.rawSystem pathStr argsStr
    Logger.log $ convert $ show ec

rm_rf :: (Logger.LoggerMonad m, MonadIO m, MonadSh m, MonadCatch m) => FilePath -> m ()
rm_rf path = case currentHost of
    Linux -> Sh.rm_rf path
    Darwin -> Sh.rm_rf path
    Windows -> do
        let removeRepeatedly = do
                exists <- Sh.test_d path
                Prologue.when_ exists $ do
                    runCommand "rmdir /s /q " path
                    let oneSecond = 1 * 1000000
                    liftIO $ threadDelay oneSecond
                    removeRepeatedly
        removeRepeatedly
        Prologue.whenM (Sh.test_e path) $ runCommand "rm " path

switchVerbosity :: Logger.LoggerMonad m => m a -> m a
switchVerbosity act = do
    opts <- view globals <$> State.get @Options
    file <- Logger.logFilePath
    let verb   = opts ^. verbose
        gui    = opts ^. guiInstaller
        logger = if verb && (not gui) then Logger.logToStdout else Logger.logToFile file
    Sh.log_stdout_with logger $ Sh.log_stderr_with logger act

-- versions of Shelly commands choosing the verbosity
-- based on the `verbose` option
run  command args = switchVerbosity $ Sh.run  command args
run_ command args = switchVerbosity $ Sh.run_ command args
