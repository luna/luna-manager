{-# LANGUAGE ScopedTypeVariables #-}
module Luna.Manager.Shell.Shelly (module Luna.Manager.Shell.Shelly, module X) where

import Prologue hiding (FilePath)

import qualified Control.Exception.Safe       as Exception
import           Control.Monad.Raise          (MonadException, raise)
import           Control.Monad.State.Layered  as State
import qualified Control.Monad.State.Lazy     as S
import           Shelly.Lifted                as X hiding (mv, run, run_, rm_rf)
import qualified Shelly.Lifted                as Sh
import           Unsafe.Coerce

import           Luna.Manager.Command.Options
import qualified Luna.Manager.Logger          as Logger
import           Luna.Manager.System.Host
import           Luna.Manager.System.Env      (EnvConfig)
import qualified Luna.Manager.System.Env      as System

import           Filesystem.Path.CurrentOS    (FilePath, (</>), encodeString, decodeString, toText, parent)
import           System.Process.Typed         as Process
import           System.Exit

deriving instance MonadSh   m => MonadSh (StateT s m)
instance          Exception e => MonadException e Sh.Sh where raise = Exception.throwM

-- Maybe we could simplify it in GHC 8.2 ?
instance MonadShControl m => MonadShControl (StateT s m) where
    newtype ShM (StateT s m) a = StateTShM { fromShM :: ShM (S.StateT s m) a }
    restoreSh shm = StateT $ restoreSh (fromShM shm)
    liftShWith (f :: ((forall x. StateT s m x -> Sh (ShM (StateT s m) x)) -> Sh a)) = StateT $ liftShWith f' where
        f' :: (forall x. S.StateT s m x -> Sh (ShM (S.StateT s m) x)) -> Sh a
        f' h = f h' where
            h' :: (forall x. StateT s m x -> Sh (ShM (StateT s m) x))
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


rm_rf :: (Logger.LoggerMonad m, MonadIO m, MonadSh m, MonadCatch m) => FilePath -> m ()
rm_rf path = case currentHost of
    Linux -> Sh.rm_rf path
    Darwin -> Sh.rm_rf path
    Windows -> do
        Prologue.whenM (Sh.test_d path) $ do
            list <- Sh.ls path
            mapM_ rm_rf list
        Prologue.whenM (Sh.test_e path) $ liftIO $ Process.runProcess_ $ Process.shell $ "powershell Remove-Item -Recurse -Force -Path " <> encodeString path

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
