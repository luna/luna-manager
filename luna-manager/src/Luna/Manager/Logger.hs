module Luna.Manager.Logger where

import           Prologue
import           Control.Monad.State.Layered
import           Data.Text                   (Text)
import qualified Data.Text.IO                as Text
import           System.IO                   (hFlush, stdout)

import           Luna.Manager.Command.Options


log :: (MonadIO m, MonadState Options m) => Text -> m ()
log msg = do
    -- TODO[piotrMocz] once we have a logging solution,
    -- the prints need to be replaced with real logging
    opts <- get @Options
    when (opts ^. globals . verbose) $ liftIO $ do
        Text.putStrLn msg
        hFlush stdout

