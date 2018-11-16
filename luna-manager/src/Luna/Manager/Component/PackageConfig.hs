module Luna.Manager.Component.PackageConfig where

import Prologue hiding (FilePath, (<.>))

import Filesystem.Path.CurrentOS      (FilePath)
import Luna.Manager.System.Host

----------------------------
-- === Package config === --
----------------------------

-- === Definition === --

data PackageConfig = PackageConfig { _defaultPackagePath :: FilePath
                                   , _buildScriptPath    :: FilePath
                                   , _thirdPartyPath     :: FilePath
                                   , _libPath            :: FilePath
                                   , _componentsToCopy   :: FilePath
                                   , _configFolder       :: FilePath
                                   , _binFolder          :: FilePath
                                   , _binsPrivate        :: FilePath
                                   , _binsPublic         :: FilePath
                                   , _mainBin            :: FilePath
                                   , _utilsFolder        :: FilePath
                                   , _logoFileName       :: Text
                                   , _desktopFileName    :: Text
                                   , _versionFileName    :: FilePath
                                   , _permitNoTags       :: Bool
                                   , _buildFromHead      :: Bool
                                   }

makeLenses ''PackageConfig



-- === Instances === --

instance Monad m => MonadHostConfig PackageConfig 'Linux arch m where
    defaultHostConfig = return $ PackageConfig
        { _defaultPackagePath = "dist-package"
        , _buildScriptPath    = "build_luna"
        , _thirdPartyPath     = "third-party"
        , _libPath            = "lib"
        , _componentsToCopy   = "dist"
        , _configFolder       = "config"
        , _binFolder          = "bin"
        , _binsPrivate        = "private"
        , _binsPublic         = "public"
        , _mainBin            = "main"
        , _utilsFolder        = "resources"
        , _logoFileName       = "logo.svg"
        , _desktopFileName    = "app.desktop"
        , _versionFileName    = "version.txt"
        , _permitNoTags       = False
        , _buildFromHead      = False
        }

instance Monad m => MonadHostConfig PackageConfig 'Darwin arch m where
    defaultHostConfig = defaultHostConfigFor @Linux

instance Monad m => MonadHostConfig PackageConfig 'Windows arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & defaultPackagePath .~ "C:\\lp"
                           & buildScriptPath    .~ "scripts_build\\build.py"
