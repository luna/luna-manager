{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Luna.Manager.Command.Install where

import Prologue hiding (txt, FilePath, toText, (<.>))

import           Luna.Manager.Archive              as Archive
import qualified Luna.Manager.Command.Options      as Opts
import           Luna.Manager.Command.Options      (Options, InstallOpts)
import           Luna.Manager.Component.Analytics  as Analytics
import           Luna.Manager.Component.Pretty
import           Luna.Manager.Component.Repository as Repo
import           Luna.Manager.Component.Version    as Version
import           Luna.Manager.Gui.DownloadProgress
import qualified Luna.Manager.Gui.Initialize       as Initilize
import           Luna.Manager.Gui.InstallationProgress
import qualified Luna.Manager.Logger               as Logger
import           Luna.Manager.Network
import           Luna.Manager.Shell.Question
import qualified Luna.Manager.Shell.Shelly         as Shelly
import           Luna.Manager.Shell.Shelly         (toTextIgnore, MonadSh, MonadShControl)
import           Luna.Manager.System               (makeExecutable, exportPath, askToExportPath, checkShell, runServicesWindows, stopServicesWindows, checkChecksum, shaUriError)
import           Luna.Manager.System.Env
import           Luna.Manager.System.Host
import           Luna.Manager.System.Path

import           Control.Concurrent                (forkIO)
import qualified Control.Exception.Safe            as Exception
import           Control.Lens.Aeson
import           Control.Monad.Raise
import           Control.Monad.State.Layered
import           Control.Monad.Trans.Resource      (MonadBaseControl)
import qualified Crypto.Hash                       as Crypto
import qualified Data.Aeson                        as JSON
import           Data.Aeson                        (ToJSON, toJSON, toEncoding, encode)
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as BSL
import qualified Data.Char                         as Char
import qualified Data.Map                          as Map
import           Data.Maybe                        (listToMaybe)
import qualified Data.Text                         as Text
import qualified Data.Text.IO                      as Text
import qualified Data.Yaml                         as Yaml
import           Filesystem.Path.CurrentOS         (FilePath, (</>), (<.>), encodeString, decodeString, toText, basename, hasExtension, parent, dropExtension)
import qualified Network.URI                       as URI
import qualified System.Directory                  as System
import qualified System.Environment                as Environment
import qualified System.Process.Typed              as Process
import           System.Exit                       (exitSuccess, exitFailure, ExitCode(..))
import           System.Info                       (arch)
import           System.IO                         (hFlush, stdout, stderr, hPutStrLn)

default(Text.Text)


---------------------------------
-- === Installation config === --
---------------------------------

-- === Definition === --

data InstallConfig = InstallConfig { _defaultConfPath        :: FilePath
                                   , _defaultBinPathBatchApp :: FilePath
                                   , _defaultBinPathGuiApp   :: FilePath
                                   , _userInfoFile           :: FilePath
                                   , _localName              :: Text
                                   , _selectedVersionPath    :: FilePath
                                   , _mainBinPath            :: FilePath
                                   , _resourcesPath          :: FilePath
                                   , _localBinPath           :: FilePath
                                   , _logoFileName           :: FilePath
                                   , _infoFileName           :: FilePath
                                   , _libPath                :: FilePath
                                   , _privateBinPath         :: FilePath
                                   , _configPath             :: FilePath
                                   , _logsFolder             :: FilePath
                                   , _thirdParty             :: FilePath
                                   }
makeLenses ''InstallConfig


-- === Instances === --


instance Monad m => MonadHostConfig InstallConfig 'Linux arch m where
    defaultHostConfig = return $ InstallConfig
        { _defaultConfPath         = "~/.luna"
        , _defaultBinPathBatchApp  = "~/.luna/bin"
        , _defaultBinPathGuiApp    = "~/.luna/bin"
        , _userInfoFile            = "~/.luna/user_info.json"
        , _localName               = "local"
        , _selectedVersionPath     = "current"
        , _mainBinPath             = "bin/main"
        , _resourcesPath           = "resources"
        , _localBinPath            = "~/.local/bin"
        , _logoFileName            = "logo.svg"
        , _infoFileName            = "app.desktop"
        , _libPath                 = "lib"
        , _privateBinPath          = "bin/private"
        , _configPath              = "config"
        , _logsFolder              = "logs"
        , _thirdParty              = "third-party"
        }

instance Monad m => MonadHostConfig InstallConfig 'Darwin arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & defaultBinPathGuiApp   .~ "/Applications"
                           & logoFileName           .~ "logo.icns"
                           & infoFileName           .~ "Info.plist"

instance Monad m => MonadHostConfig InstallConfig 'Windows arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & defaultBinPathGuiApp    .~ "C:\\Program Files\\Luna"
                           & defaultBinPathBatchApp  .~ "C:\\Program Files\\Luna"



-----------------------
-- === Installer === --
-----------------------

-- === Errors === --

newtype UnresolvedDepsError = UnresolvedDepsError [PackageHeader] deriving (Show)
makeLenses ''UnresolvedDepsError

instance Exception UnresolvedDepsError where
    displayException err = "Following dependencies were unable to be resolved: " <> show (showPretty <$> unwrap err)

type MonadInstall m = (MonadGetter Options m, MonadStates '[EnvConfig, InstallConfig, RepoConfig, MPUserData] m, MonadNetwork m, Shelly.MonadSh m, Shelly.MonadShControl m, Logger.LoggerMonad m)

-- === Utils === --

mkSystemPkgName :: Text -> Text
mkSystemPkgName = case currentHost of
    Linux   -> id
    _       -> mkCamelCaseName

mkCamelCaseName :: Text -> Text
mkCamelCaseName txt = convert $ goHead (convert txt) where
    goHead :: [Char] -> [Char]
    goHead = \case []     -> []
                   (s:ss) -> Char.toUpper s : goBody ss
    goBody = \case []       -> []
                   ('-':ss) -> goHead ss
                   (s  :ss) -> s : goBody ss

prepareInstallPath :: MonadInstall m => AppType -> FilePath -> Text -> Text -> m FilePath
prepareInstallPath appType appPath appName appVersion = expand $ case currentHost of
    Linux   -> appPath </> convert appName </> convert appVersion
    Windows -> case appType of
        GuiApp     -> appPath </> convert (mkSystemPkgName appName) </> convert appVersion
        BatchApp _ -> appPath </> convert appName </> convert appVersion
    Darwin  -> case appType of
        GuiApp     -> appPath </> convert ((mkSystemPkgName appName) <> ".app") </> "Contents" </> "Resources" </> convert appVersion
        BatchApp _ -> appPath </> convert appName </> convert appVersion

data TheSameVersionException = TheSameVersionException Version  deriving (Show)
instance Exception TheSameVersionException where
    displayException (TheSameVersionException v ) = "You have this version already installed: " <> (convert $ showPretty v)

checkIfAppAlreadyInstalledInCurrentVersion :: MonadInstall m => FilePath -> AppType -> Version -> m ()
checkIfAppAlreadyInstalledInCurrentVersion installPath appType pkgVersion = do
    guiInstaller    <- Opts.guiInstallerOpt
    testInstallPath <- Shelly.test_d installPath
    when testInstallPath $ do
        if guiInstaller then throwM $ TheSameVersionException pkgVersion
        else do
            putStrLn "You have this version already installed. Do you want to reinstall? yes/no [no]"
            ans <- liftIO $ getLine
            if ans == "yes" then do
                stopServices installPath appType
                Shelly.rm_rf installPath
            else if ans == "no" || ans == ""
            then liftIO $ exitSuccess
            else checkIfAppAlreadyInstalledInCurrentVersion installPath appType pkgVersion

data ResolvePackagePathException = ResolvePackagePathException Text deriving (Show)
instance Exception ResolvePackagePathException where
    displayException (ResolvePackagePathException file) = "Couldn't download file: " <> convert file <> " invalid URI address"

downloadIfUri :: MonadInstall m => URIPath -> m FilePath
downloadIfUri path = do
    doesFileExist <- Shelly.test_f $ fromText path
    if doesFileExist then return $ fromText path
        else if URI.isURI $ convert path
            then downloadWithProgressBar path
            else throwM $ ResolvePackagePathException path

downloadAndUnpackApp :: MonadInstall m => URIPath -> FilePath -> Text -> AppType -> Version -> m ()
downloadAndUnpackApp pkgPath installPath appName appType pkgVersion = do
    guiInstaller <- Opts.guiInstallerOpt
    checkIfAppAlreadyInstalledInCurrentVersion installPath appType pkgVersion
    stopServices installPath appType
    when guiInstaller $ downloadProgress (Progress 0 1)
    Shelly.mkdir_p $ parent installPath
    let maybePkgPathNoExtension = case currentHost of
            Linux   -> Text.stripSuffix ".AppImage" pkgPath
            Darwin  -> Text.stripSuffix ".tar.gz" pkgPath
            Windows -> if Text.isSuffixOf ".tar.gz" pkgPath
                then Text.stripSuffix ".tar.gz" pkgPath
                else Text.stripSuffix ".exe" pkgPath
        pkgShaPath = (fromMaybe pkgPath maybePkgPathNoExtension) <> ".sha256"
    pkg    <- downloadIfUri pkgPath
    pkgSha <- downloadIfUri pkgShaPath

    when guiInstaller $ installationProgress 0
    checkChecksum @Crypto.SHA256 pkg pkgSha
    unpacked <- Archive.unpack (if currentHost==Windows then 0.5 else 0.9) "installation_progress" pkg
    Logger.info $ "Copying files from " <> toTextIgnore unpacked <> " to " <> toTextIgnore installPath
    case currentHost of
         Linux   -> do
             Shelly.mkdir_p installPath
             Shelly.mv unpacked $ installPath </> convert appName
         _  -> if appType == (BatchApp Manager) then do
             Shelly.mkdir_p installPath
             Shelly.mv unpacked $ installPath </> convert appName
             else Shelly.mv unpacked installPath

linkingCurrent :: MonadInstall m => AppType -> FilePath -> m ()
linkingCurrent appType installPath = do
    installConfig <- get @InstallConfig
    let currentPath = (parent installPath) </> (installConfig ^. selectedVersionPath)
    createSymLinkDirectory installPath currentPath

makeShortcuts :: MonadInstall m => FilePath -> Text -> m ()
makeShortcuts packageBinPath appName = when (currentHost == Windows) $ do
    Logger.info "Creating Menu Start shortcut."
    bin         <- liftIO $ System.getSymbolicLinkTarget $ encodeString packageBinPath
    binAbsPath  <- Shelly.canonicalize $ (parent packageBinPath) </> (decodeString bin)
    appData     <- liftIO $ Environment.getEnv "appdata"
    let menuPrograms = (decodeString appData) </> "Microsoft" </> "Windows" </> "Start Menu" </> "Programs" </> convert ((mkSystemPkgName appName) <> ".lnk")
    exitCode <- liftIO $ Process.runProcess $ Process.shell  ("powershell" <> " \"$s=New-Object -ComObject WScript.Shell; $sc=$s.createShortcut(" <> "\'" <> (encodeString menuPrograms) <> "\'" <> ");$sc.TargetPath=" <> "\'" <> (encodeString binAbsPath) <> "\'" <> ";$sc.Save()\"")
    unless (exitCode == ExitSuccess) $ Logger.warning $ "Menu Start shortcut was not created. Powershell could not be found in the $PATH"

postInstallation :: MonadInstall m => AppType -> FilePath -> Text -> Text -> Text -> m ()
postInstallation appType installPath binPath appName version = do
    linkingCurrent appType installPath
    installConfig <- get @InstallConfig
    packageBin    <- return $ installPath </> case currentHost of
        Linux   -> convert appName
        Darwin  -> if appType == BatchApp Manager then convert appName else (installConfig ^. mainBinPath) </> convert appName
        Windows -> (installConfig ^. mainBinPath) </> convert (appName <> ".exe")
    currentBin    <- case currentHost of
        Linux   -> return $ parent installPath </> (installConfig ^. selectedVersionPath)  </> convert (mkSystemPkgName appName)
        Darwin  -> case appType of
            --TODO[1.1] lets think about making it in config
            GuiApp   -> expand $ convert binPath </> convert ((mkSystemPkgName appName) <> ".app") </> "Contents" </> "MacOS" </> convert (mkSystemPkgName appName)
            BatchApp Regular -> return $ parent installPath </> (installConfig ^. selectedVersionPath) </> (installConfig ^. mainBinPath) </> convert appName
            BatchApp Manager -> return $ parent installPath </> (installConfig ^. selectedVersionPath) </> convert appName
        Windows -> case appType of
            BatchApp _ -> return $ parent installPath </> (installConfig ^. selectedVersionPath) </> convert (appName <> ".exe")
            GuiApp     -> return $ parent installPath </> (installConfig ^. selectedVersionPath) </> (installConfig ^. mainBinPath) </> convert (appName <> ".exe")
    makeExecutable packageBin
    when (currentHost == Darwin && appType == GuiApp) $ linking packageBin currentBin
    linkingLocalBin currentBin appName
    copyResources appType installPath appName
    runServices installPath appType appName version
    when (appType==GuiApp) $ makeShortcuts packageBin appName

copyResources :: MonadInstall m => AppType -> FilePath -> Text -> m ()
copyResources appType installPath appName = when (currentHost == Darwin && appType == GuiApp) $ do
    installConfig <- get @InstallConfig
    let resources        = installPath </> (installConfig ^. mainBinPath) </> (installConfig ^. resourcesPath)
        packageLogo      = resources </> (installConfig ^. logoFileName)
        packageInfoPlist = resources </> (installConfig ^. infoFileName)
        appLogo          = parent installPath </> convert (appName <> ".icns")
        appInfoPlist     = (parent $ parent installPath) </> "Info.plist"
    logoTest      <- Shelly.test_f appLogo
    infoPlistTest <- Shelly.test_f appInfoPlist
    when logoTest      $ Shelly.rm appLogo
    when infoPlistTest $ Shelly.rm appInfoPlist
    Shelly.cp packageLogo appLogo
    Shelly.cp packageInfoPlist (parent $ parent installPath)

linking :: MonadInstall m => FilePath -> FilePath -> m ()
linking src dst = do
    Shelly.mkdir_p $ parent dst
    createSymLink src dst

linkingLocalBin :: (MonadInstall m, MonadIO m) => FilePath -> Text -> m ()
linkingLocalBin currentBin appName = do
    home          <- getHomePath
    installConfig <- get @InstallConfig
    gui           <- Opts.guiInstallerOpt
    case currentHost of
        Windows -> do
            if gui then exportPath currentBin else askToExportPath currentBin
        _       -> do
            localBin <- expand (installConfig ^. localBinPath)
            linking currentBin $ localBin </> convert appName
            if gui then exportPath localBin else askToExportPath localBin

-- === Windows specific === --

stopServices ::MonadInstall m => FilePath -> AppType -> m ()
stopServices installPath appType = when (currentHost == Windows && appType == GuiApp) $ do
    installConfig <- get @InstallConfig
    let currentServices = parent installPath </> (installConfig ^. selectedVersionPath) </> (installConfig ^. configPath) </> fromText "windows"
    do
        testservices <- Shelly.test_d currentServices
        when testservices $ stopServicesWindows currentServices

runServices :: MonadInstall m => FilePath -> AppType -> Text -> Text -> m ()
runServices installPath appType appName version = when (currentHost == Windows && appType == GuiApp) $ do
    installConfig <- get @InstallConfig
    let services = installPath </> (installConfig ^. configPath) </> fromText "windows"
    logs <- expand $ (installConfig ^. defaultConfPath) </> (installConfig ^. logsFolder) </> fromText appName </> (fromText $ showPretty version)
    runServicesWindows services logs

copyDllFilesOnWindows :: MonadInstall m => FilePath -> m ()
copyDllFilesOnWindows installPath = when (currentHost == Windows) $ do
    installConfig <- get @InstallConfig
    let libFolderPath  = installPath </> (installConfig ^. libPath)
        binsFolderPath = installPath </> (installConfig ^. privateBinPath)
    do
        listedLibs <- Shelly.ls libFolderPath
        mapM_ (`Shelly.mv` binsFolderPath) listedLibs

copyWinSW :: MonadInstall m => FilePath -> m ()
copyWinSW installPath = when (currentHost == Windows) $ do
    installConfig <- get @InstallConfig
    let winSW = installPath </> (installConfig ^. thirdParty) </> fromText "WinSW.Net4.exe"
        winConfigFolderPath = installPath </> (installConfig ^. configPath) </> fromText "windows"
    Shelly.mv winSW winConfigFolderPath

registerUninstallInfo :: MonadInstall m => FilePath -> m ()
registerUninstallInfo installPath = when (currentHost == Windows) $ do
    installConfig <- get @InstallConfig
    let registerScript = installPath </> (installConfig ^. configPath) </> fromText "windows" </> "registerUninstall.ps1"
        directory      = parent $ parent installPath -- if default, c:\Program Files\
    pkgHasRegister <- Shelly.test_f registerScript
    when pkgHasRegister $ do
        let registerPowershell = "powershell -executionpolicy bypass -file \"" <> toTextIgnore registerScript <> "\" \"" <> toTextIgnore directory <> "\""
        Logger.logProcess registerPowershell

moveUninstallScript :: MonadInstall m => FilePath -> m ()
moveUninstallScript installPath = when (currentHost == Windows) $ do
    installConfig <- get @InstallConfig
    let uninstallScript = installPath </> (installConfig ^. configPath) </> fromText "windows" </> "uninstallLunaStudio.ps1"
        rootInstallPath = parent installPath
    pkgHasUninstall <- Shelly.test_f uninstallScript
    when pkgHasUninstall $
        Shelly.cp uninstallScript $ rootInstallPath </> "uninstallLunaStudio.ps1"

prepareWindowsPkgForRunning :: MonadInstall m => FilePath -> m ()
prepareWindowsPkgForRunning installPath = do
    installConfig <- get @InstallConfig
    guiInstaller  <- Opts.guiInstallerOpt
    when guiInstaller $ installationProgress 0.7
    copyDllFilesOnWindows installPath
    when guiInstaller $ installationProgress 0.75
    copyWinSW installPath
    when guiInstaller $ installationProgress 0.8
    registerUninstallInfo installPath
    when guiInstaller $ installationProgress 0.85
    moveUninstallScript installPath

copyUserConfig :: MonadInstall m => FilePath -> ResolvedPackage -> m ()
copyUserConfig installPath package = do
    unless (currentHost == Linux) $ Logger.info "Copying user config to ~/.luna"
    installConfig <- get @InstallConfig
    let pkgName               = package ^. header . name
        pkgVersion            = showPretty $ package ^. header . version
        packageUserConfigPath = installPath </> "user-config"
    homeLunaPath      <- expand $ installConfig ^. defaultConfPath
    let homeUserConfigPath = homeLunaPath </> (installConfig ^. configPath) </> convert pkgName </> convert pkgVersion
    userConfigExists   <- Shelly.test_d packageUserConfigPath
    when userConfigExists $ do
        Shelly.rm_rf homeUserConfigPath
        Shelly.mkdir_p homeUserConfigPath
        listedPackageUserConfig <- Shelly.ls packageUserConfigPath
        mapM_ (flip Shelly.cp_r homeUserConfigPath) $ map (packageUserConfigPath </>) listedPackageUserConfig
    when (currentHost == Windows) $ do
        exitCode <- liftIO $ Process.runProcess $ Process.shell $ "attrib +h " <> encodeString homeLunaPath
        unless (exitCode == ExitSuccess) $ Logger.warning $ "Setting hidden attribute for .luna folder failed"

-- === MacOS specific === --

touchApp :: MonadInstall m => FilePath -> AppType -> m ()
touchApp appPath appType = when (currentHost == Darwin && appType == GuiApp) $ do
        Shelly.switchVerbosity $ Shelly.cmd "touch" $ toTextIgnore appPath

-- === Installation utils === --

askLocation :: (MonadStates '[EnvConfig, InstallConfig, RepoConfig] m, MonadNetwork m) => InstallOpts -> AppType -> Text -> m Text
askLocation opts appType appName = do
    installConfig <- get @InstallConfig
    let pkgInstallDefPath = case appType of
            GuiApp     -> installConfig ^. defaultBinPathGuiApp
            BatchApp _ -> installConfig ^. defaultBinPathBatchApp
    binPath <- askOrUse (opts ^. Opts.selectedInstallationPath)
        $ question ("Select installation path for " <> appName) plainTextReader
        & defArg .~ Just (toTextIgnore pkgInstallDefPath)
    return binPath

installApp :: MonadInstall m => InstallOpts -> Bool -> ResolvedPackage -> m ()
installApp opts isMainApp package = do
    installConfig <- get @InstallConfig
    guiInstaller  <- Opts.guiInstallerOpt
    let pkgName    = package ^. header . name
        appType    = package ^. resolvedAppType
    binPath     <- if guiInstaller then return $ toTextIgnore $
        case appType of
                GuiApp     -> installConfig ^. defaultBinPathGuiApp
                BatchApp _ -> installConfig ^. defaultBinPathBatchApp
        else case currentHost of
            Darwin -> askLocation opts appType pkgName
            _      -> if isMainApp then do
                path <- askLocation opts appType pkgName
                modify_ @InstallConfig (defaultBinPathGuiApp   .~ fromText path)
                modify_ @InstallConfig (defaultBinPathBatchApp .~ fromText path)
                return path
                else return $ toTextIgnore $
                    case appType of
                            GuiApp     -> installConfig ^. defaultBinPathGuiApp
                            BatchApp _ -> installConfig ^. defaultBinPathBatchApp
    installApp' binPath package

installApp' :: MonadInstall m => Text -> ResolvedPackage -> m ()
installApp' binPath package = do
    let pkgName    = package ^. header . name
        appType    = package ^. resolvedAppType
        pkgVersion = showPretty $ package ^. header . version
    installPath <- prepareInstallPath appType (convert binPath) pkgName $ pkgVersion
    downloadAndUnpackApp (package ^. desc . path) installPath pkgName appType $ package ^. header . version
    unless (appType == BatchApp Manager) $ prepareWindowsPkgForRunning installPath
    postInstallation appType installPath binPath pkgName pkgVersion
    copyUserConfig installPath package
    let appName = mkSystemPkgName pkgName <> ".app"
    touchApp (convert binPath </> convert appName) appType

data VersionException = VersionException Text  deriving (Show)
instance Exception VersionException where
    displayException (VersionException v ) = "Unknown version: " <> show v

readVersion :: (MonadIO m, MonadException SomeException m, MonadThrow m) => Text -> m Version
readVersion v = case readPretty v of
    Left e  -> throwM $ VersionException v
    Right v -> return $ v

-- Prompt user for the email, unless we already have it.
askUserEmail :: MonadIO m => m Text
askUserEmail = liftIO $ do
    putStrLn $  "Please enter your email address (it is optional"
             <> " but will help us greatly in the early beta stage):"
    Text.getLine

runApp :: MonadInstall m => Text -> Text -> AppType -> m ()
runApp appName version appType = do
    installConfig <- get @InstallConfig
    installPath <- prepareInstallPath appType (installConfig ^. defaultBinPathGuiApp) appName version
    let runPath = installPath </> (case currentHost of Linux -> ""; _ -> "bin" </> "main") </> fromText appName
    threadID <- liftIO $ forkIO $ Process.runProcess_ $ Process.shell $ encodeString $ runPath
    Logger.log $ Text.pack $ show threadID

askToRunApp :: MonadInstall m => Text -> Text -> AppType -> m ()
askToRunApp appName version appType = when (appType == GuiApp) $ do
    guiInstaller <- Opts.guiInstallerOpt
    if guiInstaller then do
        Logger.log $ "ApplicationRun json" <> (Text.pack $ show $ encode $ ApplicationRun appName)
        print $ encode $ ApplicationRun appName
        liftIO $ hFlush stdout
        doesRun <- liftIO $ BS.getLine
        Logger.log $ "ApplicationRun answear " <> (Text.pack $ show doesRun)
        let runOpt = JSON.decode $ BSL.fromStrict doesRun :: Maybe Run
        when (isJust runOpt) $ Shelly.silently $ runApp appName version appType
        print $ encode $ ApplicationClose True
        else do
            liftIO $ Text.putStrLn $  "Do you want to run " <> appName <> "? yes/no [yes]"
            ans <- liftIO $ Text.getLine
            when (ans == "yes" || ans == "") $ runApp appName version appType


-- === Running === --

run :: MonadInstall m => InstallOpts -> m ()
run opts = do
    userInfoPath <- gets @InstallConfig userInfoFile
    guiInstaller <- Opts.guiInstallerOpt
    repo <- maybe getRepo (parseConfig . convert) (opts ^. Opts.localConfig)
    if guiInstaller then do
        Initilize.generateInitialJSON repo =<< (Analytics.userInfoExists userInfoPath)
        liftIO $ hFlush stdout
        options <- liftIO $ BS.getLine

        let install = JSON.decode $ BSL.fromStrict options :: Maybe Initilize.Option
        forM_ install $ \(Initilize.Option (Initilize.Install appName appVersion emailM)) -> do
            Logger.logObject "[run] appName"    appName
            Logger.logObject "[run] appVersion" appVersion
            Analytics.mpRegisterUser userInfoPath $ fromMaybe "" emailM
            Analytics.mpTrackEvent "LunaInstaller.Started"
            appPkg           <- Logger.tryJustWithLog "Install.run" undefinedPackageError $ Map.lookup appName (repo ^. packages)
            Logger.logObject "[run] App package" appPkg
            evaluatedVersion <- Logger.tryJustWithLog "Install.run" (toException $ VersionException $ convert $ show appVersion) $ Map.lookup appVersion $ appPkg ^. versions --tryJust missingPackageDescriptionError $ Map.lookup currentSysDesc $ snd $ Map.lookup appVersion $ appPkg ^. versions
            Logger.logObject "[run] evaluated version" evaluatedVersion
            appDesc          <- Logger.tryJustWithLog "Install.run" (toException $ MissingPackageDescriptionError appVersion) $ Map.lookup currentSysDesc evaluatedVersion
            Logger.logObject "[run] app description" appDesc
            let (unresolvedLibs, pkgsToInstall) = Repo.resolve repo appDesc
            when (not $ null unresolvedLibs) $ do
                let e = UnresolvedDepsError unresolvedLibs
                Logger.exception "Install.run" $ toException e
                raise' e
            let appsToInstall = filter (( <$> (^. header . name)) (`elem` (repo ^.apps))) pkgsToInstall
                resolvedApp   = ResolvedPackage (PackageHeader appName appVersion) appDesc (appPkg ^. appType)
            installApp opts True resolvedApp
            mapM_ (installApp opts False) appsToInstall
            -- mapM_ (installApp opts) $ allApps
            print $ encode $ InstallationProgress 1
            liftIO $ hFlush stdout
            askToRunApp appName (showPretty appVersion) (appPkg ^. appType)
            Analytics.mpTrackEvent "LunaInstaller.Finished"

        else do
            Shelly.unlessM (userInfoExists userInfoPath) $ do
                email <- case (opts ^. Opts.selectedUserEmail) of
                    Just e  -> return e
                    Nothing -> askUserEmail
                Analytics.mpRegisterUser userInfoPath email

            (appName, appPkg) <- askOrUse (opts ^. Opts.selectedComponent)
                $ question "Select component to be installed" (\t -> choiceValidator' "component" t $ (t,) <$> Map.lookup t (repo ^. packages))
                & help   .~ choiceHelp "components" (repo ^. apps)
                & defArg .~ maybeHead (repo ^. apps)

            let vmap       = Map.mapMaybe (Map.lookup currentSysDesc) $ appPkg ^. versions
                filterFunc = if (opts ^. Opts.devInstallation) then const True
                             else if (opts ^. Opts.nightlyInstallation) then not . Version.isDev
                             else Version.isRelease
                vss        = filter filterFunc . sort . Map.keys $ vmap

            (appVersion, appPkgDesc) <- askOrUse (opts ^. Opts.selectedVersion)
                $ question "Select version to be installed" (\t -> choiceValidator "version" t . sequence $ fmap (t,) . flip Map.lookup vmap <$> readPretty t)
                & help   .~ choiceHelp (appName <> " versions") vss
                & defArg .~ fmap showPretty (maybeLast vss)

            let (unresolvedLibs, pkgsToInstall) = Repo.resolve repo appPkgDesc
            when (not $ null unresolvedLibs) . raise' $ UnresolvedDepsError unresolvedLibs

            version <- readVersion appVersion
            let appsToInstall = filter (( <$> (^. header . name)) (`elem` (repo ^.apps))) pkgsToInstall

                resolvedApp = ResolvedPackage (PackageHeader appName version) appPkgDesc (appPkg ^. appType)
            installApp opts True resolvedApp
            mapM_ (installApp opts False) appsToInstall

            askToRunApp appName appVersion (appPkg ^. appType)
