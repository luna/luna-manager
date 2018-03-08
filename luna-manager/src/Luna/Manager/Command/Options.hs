module Luna.Manager.Command.Options where

import Prologue

import Data.Text (Text)
import Options.Applicative as Opts
import Control.Monad.State.Layered

import qualified Luna.Manager.System.Info as Info


------------------------------
-- === Command options  === --
------------------------------

-- === Definition === --

data Options = Options
    { _globals :: GlobalOpts
    , _command :: Command
    } deriving (Show)

data GlobalOpts = GlobalOpts
    { _batchMode       :: Bool
    , _guiInstaller    :: Bool
    , _verbose         :: Bool
    , _selectedTmpPath :: Maybe Text
    } deriving (Show)

data Command = Install       InstallOpts
             | Uninstall
             | Update
             | SwitchVersion SwitchVersionOpts
             | Develop       DevelopOpts
             | MakePackage   MakePackageOpts
             | NextVersion   NextVersionOpts
             | Promote       PromoteOpts
             | Version
             deriving (Show)

data InstallOpts = InstallOpts
    { _selectedComponent        :: Maybe Text
    , _selectedVersion          :: Maybe Text
    , _selectedInstallationPath :: Maybe Text
    , _selectedUserEmail        :: Maybe Text
    , _localConfig              :: Maybe Text
    , _nightlyInstallation      :: Bool
    , _devInstallation          :: Bool
    } deriving (Show)

data MakePackageOpts = MakePackageOpts
    { _cfgPath       :: Text
    , _guiURL        :: Maybe Text
    , _permitNoTags  :: Bool
    , _buildFromHead :: Bool
    } deriving (Show)

data SwitchVersionOpts = SwitchVersionOpts
    { _targetVersion :: Text
    } deriving (Show)

data DevelopOpts = DevelopOpts
    { _target               :: Text
    , _repositoryPath       :: Maybe Text
    , _downloadDependencies :: Bool
    } deriving (Show)

data NextVersionOpts = NextVersionOpts
    { _configFilePath :: Text
    , _nightly        :: Bool
    , _release        :: Bool
    , _commit         :: Maybe Text
    } deriving (Show)

data PromoteOpts = PromoteOpts
    { _confPath  :: Text
    , _pkgPath   :: Text
    , _toRelease :: Bool
    } deriving Show

makeLenses ''GlobalOpts
makeLenses ''Options
makeLenses ''InstallOpts
makeLenses ''MakePackageOpts
makeLenses ''SwitchVersionOpts
makeLenses ''DevelopOpts
makeLenses ''NextVersionOpts
makeLenses ''PromoteOpts

-- small helpers for Options
verboseOpt, guiInstallerOpt :: MonadGetter Options m => m Bool
verboseOpt      = view (globals . verbose)      <$> get @Options
guiInstallerOpt = view (globals . guiInstaller) <$> get @Options

-- === Instances === --

instance Default InstallOpts where def = InstallOpts def def def def def False False


------------------------------
-- === Argument parsing === --
------------------------------

-- === Parsers === --

evalOptionsParserT :: MonadIO m => StateT Options m a -> m a
evalOptionsParserT m = evalStateT m =<< parseOptions

parseOptions :: MonadIO m => m Options
parseOptions = liftIO $ customExecParser (prefs showHelpOnEmpty) optsParser where
    commands           = mconcat [cmdInstall, cmdMkpkg, cmdDevelop, cmdNextVer, cmdPromote, cmdUninstall, cmdVersion]
    optsParser         = info (helper <*> optsProgram) (fullDesc <> header ("Luna ecosystem manager (" <> Info.version <> ")") <> progDesc Info.synopsis)

    -- Commands
    cmdInstall         = Opts.command "install"        . info optsInstall       $ progDesc "Install components. By default displays only the release versions."
    cmdUninstall       = Opts.command "uninstall"      . info (pure Uninstall)  $ progDesc "Uninstall Luna Studio completely"
    cmdDevelop         = Opts.command "develop"        . info optsDevelop       $ progDesc "Setup development environment"
    cmdMkpkg           = Opts.command "make-package"   . info optsMkpkg         $ progDesc "Prepare installation package"
    cmdNextVer         = Opts.command "next-version"   . info optsNextVersion   $ progDesc "Get a newer version of a package, by default incrementing the build number (x.y.z.w)"
    cmdPromote         = Opts.command "promote"        . info optsPromote       $ progDesc "Create a nightly (or release) package from a lower version without rebuilding (repackaging only)"
    cmdVersion         = Opts.command "version"        . info (pure Version)    $ progDesc "Print the version information"

    -- Options
    optsProgram        = Options           <$> optsGlobal <*> hsubparser commands
    optsGlobal         = GlobalOpts        <$> Opts.switch (long "batch"   <> help "Do not run interactive mode")
                                           <*> Opts.switch (long "gui"     <> help "Used by the graphic installer to instruct the installer it's being run in a graphical mode")
                                           <*> Opts.switch (long "verbose" <> help "Print more output from the commands ran by the manager.")
                                           <*> (optional . strOption $ long "tmp" <> metavar "TMP_PATH" <> help "Temporary folder path.")
    optsMkpkg          = MakePackage       <$> optsMkpkg'
    optsMkpkg'         = MakePackageOpts   <$> strArgument (metavar "CONFIG"  <> help "Config (luna-package.yaml) file path, usually found in the Luna Studio repo")
                                           <*> (optional . strOption $ long "gui" <> metavar "GUI_URL" <> help "Path to gui package on S3")
                                           <*> Opts.switch (long "permit-no-tags"  <> help "Do not throw an error if there is no tag for this version. Use with care.")
                                           <*> Opts.switch (long "build-from-head" <> help "Build bypassing the tag-based flow, using HEAD. Use with care.")
    optsSwitchVersion  = SwitchVersion     <$> optsSwitchVersion'
    optsSwitchVersion' = SwitchVersionOpts <$> strArgument (metavar "VERSION" <> help "Target version to switch to")
    optsDevelop        = Develop           <$> optsDevelop'
    optsDevelop'       = DevelopOpts       <$> (strArgument $ metavar "CONFIG" <> help "Config (luna-package.yaml) file path, usually found in the Luna Studio repo")
                                           <*> (optional . strOption $ long "path" <> metavar "PATH" <> help "Path under which the new repository will be created and set up.")
                                           <*> Opts.switch (long "download-dependencies" <> help "Instead of setting up the fresh repo, just download the external dependencies into the existing repo.")
    optsInstall        = Install           <$> optsInstall'
    optsInstall'       = InstallOpts       <$> (optional . strOption $ long "component" <> metavar "COMPONENT" <> help "Component to install")
                                           <*> (optional . strOption $ long "version"   <> metavar "VERSION"   <> help "Version to install")
                                           <*> (optional . strOption $ long "path"      <> metavar "PATH"      <> help "Installation path")
                                           <*> (optional . strOption $ long "email"     <> metavar "EMAIL"     <> help "Email of the user.")
                                           <*> (optional . strOption $ long "config"    <> metavar "CFG"       <> help "Local installation config")
                                           <*> Opts.switch (long "nightly" <> help "Include nightly builds in the list of builds available for installation.")
                                           <*> Opts.switch (long "dev"     <> help "Include developer builds in the list of builds available for installation.")
    optsNextVersion    = NextVersion       <$> optsNextVersion'
    optsNextVersion'   = NextVersionOpts   <$> strArgument (metavar "CONFIG" <> help "Config (luna-package.yaml) file path, usually found in the Luna Studio repo")
                                           <*> Opts.switch (long "nightly"   <> help "Get a new nightly version number (x.y.z).")
                                           <*> Opts.switch (long "release"   <> help "Get a new release version number (x.y).")
                                           <*> (optional . strOption $ long "commit" <> metavar "COMMIT" <> help "Commit hash to use as the basis for the new version")
    optsPromote        = Promote           <$> optsPromote'
    optsPromote'       = PromoteOpts       <$> strArgument (metavar "CONFIG"  <> help "Config (luna-package.yaml) file path, usually found in the Luna Studio repo")
                                           <*> strArgument (metavar "PACKAGE" <> help "The path of the package to promote.")
                                           <*> Opts.switch (long "to-release" <> help "Promote from a nightly build to a release one (default is: dev to nightly)")
