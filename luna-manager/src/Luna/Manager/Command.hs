module Luna.Manager.Command where

import Prologue

import           Control.Monad.Raise
import           Control.Monad.State.Layered
import           Control.Monad.Trans.Resource       (MonadBaseControl)
import           Luna.Manager.Command.CreatePackage (PackageConfig)
import qualified Luna.Manager.Command.CreatePackage as CreatePackage
import qualified Luna.Manager.Command.Develop       as Develop
import           Luna.Manager.Command.Install       (InstallConfig)
import qualified Luna.Manager.Command.Install       as Install
import qualified Luna.Manager.Command.NextVersion   as NextVersion
import           Luna.Manager.Command.Options
import qualified Luna.Manager.Command.Promote       as Promote
import qualified Luna.Manager.Command.Uninstall     as Uninstall
import qualified Luna.Manager.Command.Version       as Version
import           Luna.Manager.Component.Analytics   (MPUserData)
import           Luna.Manager.Component.Repository
import           Luna.Manager.Shell.Shelly          (MonadSh, MonadShControl)
import           Luna.Manager.System.Env
import           Luna.Manager.System.Host

chooseCommand :: (MonadIO m, MonadException SomeException m, MonadState Options m, MonadSh m, MonadShControl m, MonadThrow m, MonadCatch m, MonadBaseControl IO m) => m ()
chooseCommand = do
    opts <- get @Options

    case opts ^. command of
        Install     opt -> evalDefHostConfigs @'[InstallConfig, EnvConfig, RepoConfig] $ evalStateT @MPUserData (Install.run opt) def
        MakePackage opt -> evalDefHostConfigs @'[PackageConfig, EnvConfig, RepoConfig]                        $ CreatePackage.run opt
        Develop     opt -> evalDefHostConfigs @'[Develop.DevelopConfig, EnvConfig, PackageConfig, RepoConfig] $ Develop.run       opt
        NextVersion opt -> evalDefHostConfigs @'[EnvConfig, RepoConfig]                                       $ NextVersion.run   opt
        Promote     opt -> evalDefHostConfigs @'[EnvConfig, RepoConfig]                                       $ Promote.run       opt
        Uninstall       -> evalDefHostConfigs @'[InstallConfig, EnvConfig]                                    $ Uninstall.run
        Version         -> Version.run
        a               -> putStrLn $ "Unimplemented option: " ++ show a
        -- TODO: other commands
