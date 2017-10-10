{-# LANGUAGE ScopedTypeVariables #-}
module Luna.Manager.Shell.Shelly (module Luna.Manager.Shell.Shelly, module X) where

import Prologue hiding (FilePath)

import           Control.Monad.State.Layered as State
import qualified Control.Monad.State.Lazy    as S
import           Shelly.Lifted               as X hiding (mv, run, run_)
import qualified Shelly.Lifted               as S
import           Unsafe.Coerce

import           Luna.Manager.Command.Options
import           Luna.Manager.System.Host


deriving instance MonadSh        m => MonadSh        (StateT s m)

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
    Windows -> S.mv src dst

switchVerbosity :: (MonadState Options m, MonadShControl m) => m a -> m a
switchVerbosity act = do
    opts <- State.get @Options
    S.print_commands (opts ^. globals . verbose) act

-- versions of Shelly commands choosing the verbosity
-- based on the `verbose` option
run  command args = switchVerbosity $ S.run  command args
run_ command args = switchVerbosity $ S.run_ command args
