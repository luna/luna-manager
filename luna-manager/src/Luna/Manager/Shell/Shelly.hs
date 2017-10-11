{-# LANGUAGE ScopedTypeVariables #-}
module Luna.Manager.Shell.Shelly (module Luna.Manager.Shell.Shelly, module X) where

import Prologue hiding (FilePath)

import qualified Control.Exception.Safe       as Exception
import           Control.Monad.Raise          (MonadException, raise)
import           Control.Monad.State.Layered  as State
import qualified Control.Monad.State.Lazy     as S
import           Shelly.Lifted                as X hiding (mv, run, run_)
import qualified Shelly.Lifted                as Sh
import           Unsafe.Coerce

import           Luna.Manager.Command.Options
import qualified Luna.Manager.Logger          as Logger
import           Luna.Manager.System.Host
import           Luna.Manager.System.Env      (EnvConfig)
import qualified Luna.Manager.System.Env      as System

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


mv :: (MonadIO m, MonadSh m) => FilePath -> FilePath -> m ()
mv src dst = case currentHost of
    Linux   -> cmd "mv" src dst
    Darwin  -> cmd "mv" src dst
    Windows -> Sh.mv src dst

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
