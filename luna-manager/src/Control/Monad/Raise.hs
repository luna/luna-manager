{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Raise (module Control.Monad.Raise, module X) where

-- FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
-- FIXME [WD]: refactor the whole file out before the release

import Data.Kind
import Prelude

import Control.Exception as X (Exception, SomeException, toException)

import qualified Control.Exception.Safe      as Exception
import qualified Control.Monad.State.Layered as State
import qualified Control.Monad.State.Strict  as S

import Control.Monad               (join)
import Control.Monad.Catch         (MonadThrow, throwM)
import Control.Monad.Catch         (MonadCatch (..))
import Control.Monad.Trans         (MonadTrans, lift)
import Control.Monad.Trans.Control (ComposeSt (..), MonadBaseControl (..),
                                    MonadTransControl (..), defaultLiftBaseWith,
                                    defaultLiftWith, defaultRestoreM,
                                    defaultRestoreT)
import Control.Monad.Trans.Except  (ExceptT, runExceptT, throwE)
import Data.Constraint             (Constraint)

import Control.Lens.Utils


-------------------------------
-- === Exception raising === --
-------------------------------

-- === MonadException === --

type ExceptT' = ExceptT SomeException

class (Monad m, Exception e) => MonadException e m where
    raise :: forall a. e -> m a

type family MonadExceptions es m :: Constraint where
    MonadExceptions '[]       m = ()
    MonadExceptions (e ': es) m = (MonadException e m, MonadExceptions es m)


-- === Utils === --

handle :: Monad m => (e -> m a) -> ExceptT e m a -> m a
handle f = join . fmap (either f pure) . runExceptT

handleAll :: Monad m => (SomeException -> m a) -> ExceptT' m a -> m a
handleAll = handle

rethrow :: (MonadThrow m, Exception e) => ExceptT e m a -> m a
rethrow = handle throwM

rethrowAll :: MonadThrow m => ExceptT' m a -> m a
rethrowAll = rethrow

tryAll :: ExceptT' m a -> m (Either SomeException a)
tryAll = runExceptT


-- === Throws === --

type family   Throws (e :: k) (m :: * -> *) :: Constraint
type instance Throws e m = MonadExceptions e m
type instance Throws e m = MonadException  e m


-- === Intsances === --

-- Default MonadException instances
instance {-# OVERLAPPABLE #-} (Monad m, Monad (t m), MonadTrans t, MonadException e m)
                                                     => MonadException e (t                     m) where raise = lift . raise
instance {-# OVERLAPPABLE #-} (Monad m, Exception e) => MonadException e (ExceptT e             m) where raise = throwE
instance {-# OVERLAPPABLE #-} (Monad m, Exception e) => MonadException e (ExceptT SomeException m) where raise = throwE . toException
instance                      (Monad m)              => MonadException SomeException (ExceptT SomeException m) where raise = throwE
instance                      Exception e            => MonadException e IO where raise = Exception.throwM
-- deriving instance MonadCatch m => MonadCatch (StateT t m)
-- instance MonadBaseControl b m => MonadBaseControl b (StateT t m) where
--     type StM (StateT t m) a = ComposeSt (StateT t) m a
--     liftBaseWith f   = defaultLiftBaseWith f
--     restoreM         = defaultRestoreM
-- instance MonadTransControl (StateT t) where
--     type StT (StateT t) a = StT (S.StateT t) a
--     liftWith = defaultLiftWith StateT (\(StateT s) -> s)
--     restoreT = defaultRestoreT StateT
-- === Utils === --

tryJust :: MonadException e m => e -> Maybe a -> m a
tryJust e = maybe (raise e) pure

tryRight :: MonadException e m => (l -> e) -> Either l r -> m r
tryRight f = \case
    Right r -> pure r
    Left  l -> raise $ f l

tryRight' :: forall l m r. (MonadException SomeException m, Exception l) => Either l r -> m r
tryRight' = tryRight toException

raise' :: (MonadException SomeException m, Exception e) => forall a. e -> m a
raise' = raise . toException
