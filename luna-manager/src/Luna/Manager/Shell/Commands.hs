module Luna.Manager.Shell.Commands where

import Prologue hiding (FilePath)

import Control.Monad.Raise


import Luna.Manager.Command.Options (Options)
import Luna.Manager.Shell.Shelly    (MonadSh, MonadShControl)
import Luna.Manager.System.Env      (EnvConfig)

import qualified Control.Monad.State.Layered as State
import qualified Data.Text                   as Text
import qualified Luna.Manager.Shell.Shelly   as Shelly


newtype CmdError = CmdError Text deriving (Show)
instance Exception CmdError where
    displayException (CmdError t) = convert $ "External command error: " <> t

type CmdContext m = (MonadIO m, MonadSh m, MonadShControl m, MonadException SomeException m, State.Getters '[Options, EnvConfig] m)

cmdEither :: CmdContext m => Shelly.FilePath -> [Text] -> m (Either CmdError Text)
cmdEither name args = do
    out <- Shelly.run name args
    err <- Shelly.lastStderr
    pure $ if err /= "" then Left $ CmdError err
                          else Right out

cmd :: CmdContext m => Shelly.FilePath -> [Text] -> m Text
cmd name args = tryRight' =<< cmdEither name args
