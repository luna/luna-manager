{-# LANGUAGE ExtendedDefaultRules #-}
module Luna.Manager.Command.CreatePackage where

import Prologue hiding (FilePath, (<.>))

import           Control.Lens.Aeson                   ()
import           Control.Monad                        (forM)
import qualified Control.Monad.State.Layered          as State
import           Data.Maybe                           (maybeToList)
import           Filesystem.Path.CurrentOS            (FilePath, dirname,
                                                       encodeString, extension,
                                                       filename, null, parent,
                                                       splitDirectories, (<.>),
                                                       (</>))
import           Luna.Manager.Command.Options         (MakePackageOpts, Options,
                                                       guiInstallerOpt)
import           Luna.Manager.Component.PackageConfig
import           Luna.Manager.Component.Pretty
import           Luna.Manager.Component.Version       (Version, readVersion)
import           Luna.Manager.Network
import           Luna.Manager.Shell.Shelly            (MonadSh)
import           Luna.Manager.System                  (generateChecksum,
                                                       makeExecutable)
import           Luna.Manager.System.Env
import           Luna.Manager.System.Host
import           Luna.Manager.System.Path
import           System.Exit

import qualified Control.Exception.Safe                 as Exception
import qualified Crypto.Hash                            as Crypto
import qualified Data.ByteString.Lazy.Char8             as BSLChar
import qualified Data.Text                              as Text
import qualified Filesystem.Path.CurrentOS              as FP
import qualified Luna.Manager.Archive                   as Archive
import qualified Luna.Manager.Command.Options           as Opts
import qualified Luna.Manager.Component.Repository      as Repository
import qualified Luna.Manager.Component.WindowsResource as WindowsResource
import qualified Luna.Manager.Logger                    as Logger
import qualified Luna.Manager.Shell.Shelly              as Shelly
import qualified Safe
import qualified System.Process.Typed                   as Process

default (Text.Text)


type MonadCreatePackage m =
    ( State.Getter Options m
    , State.MonadStates '[EnvConfig, PackageConfig, Repository.RepoConfig] m
    , MonadNetwork m
    , MonadSh m
    , Shelly.MonadShControl m
    , MonadIO m
    )


data NoAppException = NoAppException deriving(Show)
instance Exception NoAppException  where
    displayException NoAppException = "No Applicattions defined to create package."

data AppimageException = AppimageException SomeException deriving (Show)
instance Exception AppimageException where
    displayException (AppimageException exception ) = "AppImage not created because of: " <> displayException exception

data ExistingVersionException = ExistingVersionException Version deriving (Show)
instance Exception ExistingVersionException where
    displayException (ExistingVersionException v) = "This version already exists: " <> (convert $ showPretty v)

data NoTagException = NoTagException Text deriving (Show)
instance Exception NoTagException where
    displayException (NoTagException t) = "The tag " <> (convert t) <> " does not exist in the repo"

----------------------
-- === Appimage === --
----------------------

modifyDesktopFileToUseWrapperAppImageToRunApp :: MonadCreatePackage m => Text -> FilePath -> m ()
modifyDesktopFileToUseWrapperAppImageToRunApp appName tmpAppDirPath =
    Process.runProcess_ $ Process.setWorkingDir (encodeString tmpAppDirPath) $ Process.shell $ inPlaceSubs <> substitute <> desktopFile
    where desktopFile     = convert appName <> ".desktop"
          wrappedExecName = convert appName <> ".wrapper"
          substitute      = "\"s|Exec=" <> (convert appName) <> "|Exec=" <> wrappedExecName <> "|g\" "
          inPlaceSubs     = "sed -i -e "

copyResourcesAppImage :: MonadCreatePackage m => FilePath -> Text -> FilePath -> FilePath -> m ()
copyResourcesAppImage repoPath appName tmpAppDirPath mainAppImageFolderPath = do
    pkgConfig  <- State.get @PackageConfig
    srcPkgPath <- expand $ repoPath </> (pkgConfig ^. defaultPackagePath) </> convert appName
    let utilsPath   = srcPkgPath </> (pkgConfig ^. binFolder) </> (pkgConfig ^. mainBin) </> (pkgConfig ^. utilsFolder)
        logoFile    = utilsPath </> convert (pkgConfig ^. logoFileName)
        desktopFile = utilsPath </> convert (pkgConfig ^. desktopFileName)
    Shelly.cp logoFile    $ tmpAppDirPath </> convert (appName <> ".svg")
    Shelly.cp desktopFile $ tmpAppDirPath </> convert (appName <> ".desktop")
    copyDir srcPkgPath mainAppImageFolderPath

checkAppImageName :: MonadCreatePackage m => Text -> Version -> FilePath -> m FilePath
checkAppImageName appName version filePath = do
    let fileName      = filename filePath
        outFolderPath = parent $ filePath
        fullAppImagePath = outFolderPath </> convert (finalPackageName appName version <> ".AppImage")
    when (Text.isInfixOf appName (Shelly.toTextIgnore fileName)) $ do
        Shelly.mv filePath fullAppImagePath
    pure fullAppImagePath

changeAppImageName :: MonadCreatePackage m => Text -> Version -> FilePath -> m FilePath
changeAppImageName appName version outFolderPath = do
    listedDir <- Shelly.ls outFolderPath
    appimagesList <- mapM (checkAppImageName appName version) listedDir
    pure $ Safe.headDef (outFolderPath </> convert (appName <> ".AppImage")) appimagesList

getApprun :: MonadCreatePackage m => FilePath -> FilePath -> m ()
getApprun tmpAppDirPath functions = do
    let apprun = "get_apprun"
    (exitCode, out, err) <- Process.readProcess $ Process.setWorkingDir (encodeString tmpAppDirPath) $ Process.shell $ ". " <> (encodeString functions) <> " && " <> apprun
    unless (exitCode == ExitSuccess) $ throwM (AppimageException (toException $ Exception.StringException (BSLChar.unpack err) callStack))

generateAppimage :: MonadCreatePackage m => FilePath -> FilePath -> Text -> m ()
generateAppimage tmpAppPath functions appName = do
    Logger.log "Generating app image"
    let generateAppimage   = "generate_type2_appimage"
    (exitCode, out, err) <- Process.readProcess $ Process.setWorkingDir (encodeString tmpAppPath) $ Process.setEnv [("APP", (convert appName))] $ Process.shell $ ". " <> (encodeString functions) <> " && " <> generateAppimage
    unless (exitCode == ExitSuccess) $ throwM (AppimageException (toException $ Exception.StringException (BSLChar.unpack err) callStack))

createAppimage :: MonadCreatePackage m => Text -> Version -> FilePath -> m FilePath
createAppimage appName version repoPath = do
    Logger.log "Creating app image"
    let appImageFolderName = "appimage"
    pkgConfig     <- State.get @PackageConfig
    tmpAppPath    <- expand $ repoPath </> (pkgConfig ^. defaultPackagePath) </> appImageFolderName </> convert appName
    let tmpAppDirPath = tmpAppPath </> convert (appName <> ".AppDir")
    doesTmpExist <- Shelly.test_d tmpAppDirPath
    when doesTmpExist $ Shelly.rm_rf $ parent tmpAppPath
    Shelly.mkdir_p tmpAppDirPath

    Logger.log "Downloading AppImage functions.sh"
    functions <- downloadWithProgressBarTo "https://github.com/probonopd/AppImages/raw/master/functions.sh" tmpAppPath
    let mainAppImageFolder     = "usr"
        mainAppImageFolderPath = tmpAppDirPath </> mainAppImageFolder
    Shelly.mkdir_p mainAppImageFolderPath

    getApprun tmpAppDirPath functions
    copyResourcesAppImage repoPath appName tmpAppDirPath mainAppImageFolderPath

    Logger.log "Downloading AppImage desktopIntegration"
    appWrapper <- downloadWithProgressBarTo "https://raw.githubusercontent.com/probonopd/AppImageKit/master/desktopintegration" tmpAppDirPath
    let dstWrapperPath = mainAppImageFolderPath </> convert (appName <> ".wrapper")
    Shelly.mv appWrapper dstWrapperPath
    makeExecutable dstWrapperPath
    modifyDesktopFileToUseWrapperAppImageToRunApp appName tmpAppDirPath

    generateAppimage tmpAppPath functions appName

    let outFolder = (parent $ tmpAppPath) </> "out"
    changeAppImageName appName version outFolder

------------------------------
-- === Package building === --
------------------------------

-- === Utils === --


finalPackageName :: Text -> Version -> Text
finalPackageName appName version = appName <> "-" <> showPretty currentHost <> "-" <> showPretty version

runPkgBuildScript :: MonadCreatePackage m => FilePath -> Maybe Text -> Bool -> m ()
runPkgBuildScript repoPath s3GuiURL dryRun = do
    Logger.log "Running package build script"
    pkgConfig <- State.get @PackageConfig
    buildPath <- expand $ repoPath </> (pkgConfig ^. buildScriptPath)
    let guiUrlArgs = maybeToList $ ("--gui_url=" <>) <$> s3GuiURL
        dryRunArgs = if dryRun then ["--dry-run"] else []
        argList    = "--release" : (dryRunArgs <> guiUrlArgs)
    Shelly.chdir (parent buildPath) $ Shelly.switchVerbosity $
        case currentHost of
            Windows -> Shelly.cmd "py" buildPath argList
            _       -> Shelly.run_     buildPath argList

removeGitFolders :: MonadCreatePackage m => FilePath -> m ()
removeGitFolders path = do
    Prologue.whenM (Shelly.test_d path) $ do
        list <- Shelly.ls path
        mapM_ removeGitFolders list
    when (dirname path == ".git") $ Shelly.rm_rf path

copyFromDistToDistPkg :: MonadCreatePackage m => Text -> FilePath -> m ()
copyFromDistToDistPkg appName repoPath = do
    Logger.log "Copying from dist to dist-package"
    pkgConfig         <- State.get @PackageConfig
    packageRepoFolder <- case currentHost of
        Windows -> pure $ (pkgConfig ^. defaultPackagePath) </> convert appName
        _       -> expand $ repoPath </> (pkgConfig ^. defaultPackagePath) </> convert appName
    let expandedCopmponents = repoPath </> (pkgConfig ^. componentsToCopy)
    Shelly.rm_rf packageRepoFolder
    Shelly.mkdir_p $ parent packageRepoFolder
    Shelly.cp_r expandedCopmponents packageRepoFolder
    removeGitFolders packageRepoFolder

downloadAndUnpackDependency :: MonadCreatePackage m => FilePath -> Repository.ResolvedPackage -> m ()
downloadAndUnpackDependency repoPath resolvedPackage = do
    pkgConfig <- State.get @PackageConfig
    let depName          = resolvedPackage ^. Repository.header . Repository.name
        packageType      = resolvedPackage ^. Repository.resolvedAppType
        componentsFolder = pkgConfig ^. componentsToCopy
    thirdPartyFullPath <- expand $ repoPath </> componentsFolder </> (pkgConfig ^. thirdPartyPath)
    libFullPath        <- expand $ repoPath </> componentsFolder </> (pkgConfig ^. libPath)
    downloadedPkg      <- downloadFromURL (resolvedPackage ^. Repository.desc . Repository.path) $ "Downloading dependency files " <> depName
    unpacked           <- Archive.unpack 1.0 "unpacking_progress" downloadedPkg Nothing
    unpackedIsDir      <- Shelly.test_d unpacked
    Shelly.mkdir_p thirdPartyFullPath
    case packageType of
        Repository.Lib -> do
            Shelly.rm_rf libFullPath
            if unpackedIsDir then do
                listed <- Shelly.ls unpacked
                if length listed == 1 then do
                    listedIsDir <- Shelly.test_d $ unsafeHead listed -- FIXME
                    if listedIsDir then
                        mapM_ (flip Shelly.mv libFullPath) listed
                        else Shelly.mv unpacked libFullPath
                    else Shelly.mv unpacked libFullPath
                else Shelly.mv unpacked libFullPath
        _ -> do
            Shelly.rm_rf $ thirdPartyFullPath </> (unsafeLast $ splitDirectories unpacked) -- FIXME
            Shelly.mv unpacked thirdPartyFullPath


getRepoPath :: FilePath -> Repository.ResolvedApplication -> FilePath
getRepoPath cfgFolderPath resolvedApp = if desc ^. Repository.path == "./"
        then cfgFolderPath
        else convert (desc ^. Repository.path)
    where desc = resolvedApp ^. Repository.resolvedApp . Repository.desc

downloadExternalPkgs :: MonadCreatePackage m
                     => FilePath -> Repository.ResolvedApplication
                     -> MakePackageOpts -> m [FilePath]
downloadExternalPkgs cfgFolderPath resolvedApp opts = do
    let repoPath = getRepoPath cfgFolderPath resolvedApp
        tgtPath  = repoPath </> "env/stdlib"
        pkgUrls = opts ^. Opts.extPkgUrls

    pkgs <- forM pkgUrls $ \pu -> do
        archive     <- downloadFromURL pu "External package: "
        let pkgName = FP.fromText $ Text.takeWhile (/= '-')
                    $ Shelly.toTextIgnore $ FP.basename $ FP.filename archive
        folder      <- Archive.unpack 1.0 "unpacking_progress" archive (Just pkgName)
        case extension archive of
            Just "gz" -> do
                liftIO . putStrLn $ "Copying from: " <> show folder <> " into " <> show tgtPath
                Shelly.cp_r folder tgtPath
                pure [tgtPath </> folder]
            _ -> do
                dirContents <- Shelly.ls folder
                forM dirContents $ \f -> do
                    Shelly.cp_r f tgtPath
                    pure $ tgtPath </> filename f

    pure $ concat pkgs

------------------------------
-- === linkingLibsMacOS === --
------------------------------

isSubPath :: Text -> Text -> Bool
isSubPath systemLibPath dylibPath = (firstL /= systemSplited) && (not $ Filesystem.Path.CurrentOS.null $ convert dylibPath)
    where dylibSplited  = splitDirectories $ convert dylibPath
          systemSplited = splitDirectories $ convert systemLibPath
          l             = length systemSplited
          firstL        = take l dylibSplited

changeExecutableLibPathToRelative :: MonadCreatePackage m => FilePath -> FilePath -> FilePath -> m ()
changeExecutableLibPathToRelative binPath libSystemPath libLocalPath = do
    let dylibName           = filename libSystemPath
        relativeLibraryPath = "@executable_path/../../lib/" <> Shelly.toTextIgnore dylibName
        binFolder           = parent binPath
        binName             = "./"  <> (Shelly.toTextIgnore $ filename binPath)
    when (filename libLocalPath == filename libSystemPath) $ do
        Shelly.chdir binFolder $ Shelly.switchVerbosity $ do
            Shelly.cmd "install_name_tool" "-change" libSystemPath relativeLibraryPath binName

changeExecutablesLibPaths :: MonadCreatePackage m => FilePath -> FilePath -> FilePath -> m ()
changeExecutablesLibPaths binaryPath librariesFolderPath linkedDylib = do
    listedLibrariesFolder <- Shelly.ls librariesFolderPath
    mapM_ (changeExecutableLibPathToRelative binaryPath linkedDylib) listedLibrariesFolder


checkAndChangeExecutablesLibPaths :: MonadCreatePackage m => FilePath -> FilePath -> m()
checkAndChangeExecutablesLibPaths libFolderPath binaryPath = do
    Logger.log "Running otool"
    deps <- Shelly.switchVerbosity $ Shelly.cmd "otool" "-L" binaryPath
    let splited                 = drop 1 $ Text.strip <$> Text.splitOn "\n" deps
        filePaths               = Text.takeWhile (/= ' ') <$> splited
        filtered                = convert <$> filterSystemLibraries filePaths
        filterSystemLibraries s = filter checkIfSystemLibrary s
        checkIfSystemLibrary    = isSubPath "/usr/lib/"

    mapM_ (changeExecutablesLibPaths binaryPath libFolderPath) filtered

linkLibs :: MonadCreatePackage m => FilePath -> FilePath -> m ()
linkLibs binPath libPath = do
    allBins <- Shelly.ls binPath
    mapM_ (checkAndChangeExecutablesLibPaths libPath) (filterGitKeepFile allBins)

filterGitKeepFile :: [FilePath] -> [FilePath]
filterGitKeepFile allBins = filter (\x -> filename x /= ".gitkeep") allBins

-------------------------------
-- === Creating package === ---
-------------------------------


createPkg :: MonadCreatePackage m
          => FilePath -> Maybe Text -> Repository.ResolvedApplication -> Bool
          -> m ()
createPkg cfgFolderPath s3GuiURL resolvedApplication dryRun = do
    pkgConfig <- State.get @PackageConfig
    let app        = resolvedApplication ^. Repository.resolvedApp
        appPath    = getRepoPath cfgFolderPath resolvedApplication
        appHeader  = app ^. Repository.header
        appName    = appHeader ^. Repository.name
        appType    = app ^. Repository.resolvedAppType
        appVersion = appHeader ^. Repository.version

    Logger.log $ "Creating version: " <> (showPretty appVersion)

    unless dryRun $ do
        let deps = resolvedApplication ^. Repository.pkgsToPack
        mapM_ (downloadAndUnpackDependency appPath) deps

    repo <- expand $ appPath
    let componentsFolder = pkgConfig ^. componentsToCopy
        versionFile = repo </> componentsFolder </> (pkgConfig ^. configFolder) </> (pkgConfig ^. versionFileName)

    Shelly.mkdir_p $ parent versionFile
    liftIO $ writeFile (encodeString versionFile) $ convert $ showPretty appVersion

    runPkgBuildScript appPath s3GuiURL dryRun
    copyFromDistToDistPkg appName appPath
    mainAppDir <- case currentHost of
        Windows -> pure $ (pkgConfig ^. defaultPackagePath) </> convert appName
        _       -> expand $ appPath </> (pkgConfig ^. defaultPackagePath) </> convert appName
    let binsFolder = mainAppDir </> (pkgConfig ^. binFolder)
        privateBinsFolder = binsFolder </> (pkgConfig ^. binsPrivate)
        publicBinsFolder  = binsFolder </> (pkgConfig ^. binsPublic)
        libsFolder        = mainAppDir </> (pkgConfig ^. libPath)

    when (currentHost == Windows) $ do
        let appName' = convert appName
            binary   = publicBinsFolder </> appName' </> appName' <.> "exe"
        WindowsResource.updateWindowsMetadata appVersion privateBinsFolder binary
        WindowsResource.signWindowsBinaries privateBinsFolder binary

    when (currentHost == Darwin) $
        Shelly.silently $ linkLibs privateBinsFolder libsFolder

    package <- case currentHost of
                  Linux -> createAppimage appName appVersion appPath
                  _     -> Archive.pack mainAppDir $ finalPackageName appName appVersion

    Logger.log $ "Generated package: " <> (convert $ show package)

    generateChecksum  @Crypto.SHA256 package


run :: MonadCreatePackage m => MakePackageOpts -> m ()
run opts = do
    guiInstaller <- guiInstallerOpt
    version      <- readVersion (opts ^. Opts.pkgVersion)
    config       <- Repository.parseConfig $ convert (opts ^. Opts.cfgPath)

    let cfgFolderPath = parent $ convert (opts ^. Opts.cfgPath)
        appToPack     = Safe.headMay $ config ^. Repository.apps
        s3GuiUrl      = opts ^. Opts.guiURL
    case appToPack of
        Nothing -> throwM NoAppException
        Just a -> do
            resolved <- Repository.resolvePackageApp config a
            let resolvedWithVersion = resolved & Repository.resolvedApp . Repository.header . Repository.version .~ version
            pkgs <- downloadExternalPkgs cfgFolderPath resolved opts
            createPkg cfgFolderPath s3GuiUrl resolvedWithVersion (opts ^. Opts.dryRun)

            repo <- Repository.getRepo
            let updateConfig = Repository.updateConfig config resolvedWithVersion
            Repository.generateConfigYamlWithNewPackage repo updateConfig $ cfgFolderPath </> "config.yaml"

            forM_ pkgs Shelly.rm_rf
