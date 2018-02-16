{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Command.CreatePackage where

import           Control.Lens.Aeson
import           Control.Monad.Raise
import           Control.Monad.State.Layered
import           Filesystem.Path.CurrentOS (FilePath, (</>), encodeString, decodeString, parent, splitDirectories, null, filename, dirname, splitDirectories)
import           Luna.Manager.Archive as Archive
import           Luna.Manager.Command.Options (Options, MakePackageOpts, guiInstallerOpt)
import qualified Luna.Manager.Logger as Logger
import           Luna.Manager.Component.Pretty
import           Luna.Manager.Component.Repository as Repo
import           Luna.Manager.Network
import           Luna.Manager.System            (makeExecutable, generateChecksum)
import           Luna.Manager.System.Env
import           Luna.Manager.System.Host
import           Luna.Manager.System.Path
import           Luna.Manager.Component.Version (Version)
import           Luna.Manager.Component.Pretty
import           Prologue                       hiding (FilePath)
import qualified Crypto.Hash                    as Crypto
import qualified Data.Map                       as Map
import           Data.Maybe                     (maybeToList)
import qualified Data.Text                      as Text
import qualified Data.Yaml                      as Yaml
import qualified Luna.Manager.Command.Options   as Opts
import qualified Luna.Manager.Shell.Shelly      as Shelly
import qualified Safe                           as Safe
import qualified System.Process.Typed           as Process
import System.Exit
import System.Directory                         (renameDirectory)
import Luna.Manager.Shell.Shelly                (MonadSh)
import qualified Control.Exception.Safe         as Exception
import qualified Data.ByteString.Lazy.Char8     as BSLChar

import qualified Data.Text as T
default (T.Text)

----------------------------
-- === Package config === --
----------------------------

-- === Definition === --

data PackageConfig = PackageConfig { _defaultPackagePath     :: FilePath
                                   , _buildScriptPath        :: FilePath
                                   , _thirdPartyPath         :: FilePath
                                   , _libPath                :: FilePath
                                   , _componentsToCopy       :: FilePath
                                   , _configFolder           :: FilePath
                                   , _binFolder              :: FilePath
                                   , _binsPrivate            :: FilePath
                                   , _mainBin                :: FilePath
                                   , _utilsFolder            :: FilePath
                                   , _logoFileName           :: Text
                                   , _desktopFileName        :: Text
                                   , _versionFileName        :: FilePath
                                   , _permitNoTags           :: Bool
                                   , _buildFromHead          :: Bool
                                   }

makeLenses ''PackageConfig

type MonadCreatePackage m = (MonadGetter Options m, MonadStates '[EnvConfig, PackageConfig, RepoConfig] m, MonadNetwork m, MonadSh m, Shelly.MonadShControl m, MonadIO m)


-- === Instances === --

instance Monad m => MonadHostConfig PackageConfig 'Linux arch m where
    defaultHostConfig = return $ PackageConfig
        { _defaultPackagePath = "dist-package"
        , _buildScriptPath    = "build"
        , _thirdPartyPath     = "third-party"
        , _libPath            = "lib"
        , _componentsToCopy   = "dist"
        , _configFolder       = "config"
        , _binFolder          = "bin"
        , _binsPrivate        = "private"
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
    pkgConfig  <- get @PackageConfig
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
    return fullAppImagePath

changeAppImageName :: MonadCreatePackage m => Text -> Version -> FilePath -> m FilePath
changeAppImageName appName version outFolderPath = do
    listedDir <- Shelly.ls outFolderPath
    appimagesList <- mapM (checkAppImageName appName version) listedDir
    return $ Safe.headDef (outFolderPath </> convert (appName <> ".AppImage")) appimagesList

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
    pkgConfig     <- get @PackageConfig
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

runPkgBuildScript :: MonadCreatePackage m => FilePath -> Maybe Text -> m ()
runPkgBuildScript repoPath s3GuiURL = do
    Logger.log "Running package build script"
    pkgConfig <- get @PackageConfig
    buildPath <- expand $ repoPath </> (pkgConfig ^. buildScriptPath)
    let guiUrl = maybeToList $ ("--gui_url=" <>) <$> s3GuiURL
    Shelly.chdir (parent buildPath) $ Shelly.switchVerbosity $
        case currentHost of
            Windows -> Shelly.cmd "py" buildPath $ ["--release"] ++ guiUrl
            _       -> Shelly.run_ buildPath $ ["--release"] ++ guiUrl

removeGitFolders :: MonadCreatePackage m => FilePath -> m ()
removeGitFolders path = do
    Prologue.whenM (Shelly.test_d path) $ do
        list <- Shelly.ls path
        mapM_ removeGitFolders list
    when (dirname path == ".git") $ Shelly.rm_rf path

copyFromDistToDistPkg :: MonadCreatePackage m => Text -> FilePath -> m ()
copyFromDistToDistPkg appName repoPath = do
    Logger.log "Copying from dist to dist-package"
    pkgConfig         <- get @PackageConfig
    packageRepoFolder <- case currentHost of
        Windows -> return $ (pkgConfig ^. defaultPackagePath) </> convert appName
        _       -> expand $ repoPath </> (pkgConfig ^. defaultPackagePath) </> convert appName
    let expandedCopmponents = repoPath </> (pkgConfig ^. componentsToCopy)
    Shelly.rm_rf packageRepoFolder
    Shelly.mkdir_p $ parent packageRepoFolder
    Shelly.mv expandedCopmponents packageRepoFolder
    removeGitFolders packageRepoFolder

downloadAndUnpackDependency :: MonadCreatePackage m => FilePath -> ResolvedPackage -> m ()
downloadAndUnpackDependency repoPath resolvedPackage = do
    pkgConfig <- get @PackageConfig
    let depName          = resolvedPackage ^. header . name
        packageType      = resolvedPackage ^. resolvedAppType
        componentsFolder = pkgConfig ^. componentsToCopy
    thirdPartyFullPath <- expand $ repoPath </> componentsFolder </> (pkgConfig ^. thirdPartyPath)
    libFullPath        <- expand $ repoPath </> componentsFolder </> (pkgConfig ^. libPath)
    downloadedPkg      <- downloadFromURL (resolvedPackage ^. desc . path) $ "Downloading dependency files " <> depName
    unpacked           <- Archive.unpack 1.0 "unpacking_progress" downloadedPkg
    unpackedIsDir      <- Shelly.test_d unpacked
    Shelly.mkdir_p thirdPartyFullPath
    case packageType of
        Lib -> do
            Shelly.rm_rf libFullPath
            if unpackedIsDir then do
                listed <- Shelly.ls unpacked
                if length listed == 1 then do
                    listedIsDir <- Shelly.test_d $ head listed
                    if listedIsDir then
                        mapM_ (flip Shelly.mv libFullPath) listed
                        else Shelly.mv unpacked libFullPath
                    else Shelly.mv unpacked libFullPath
                else Shelly.mv unpacked libFullPath
        _ -> do
            Shelly.rm_rf $ thirdPartyFullPath </> (last $ splitDirectories unpacked)
            Shelly.mv unpacked thirdPartyFullPath

isNewestVersion :: MonadCreatePackage m => Version -> Text -> m Bool
isNewestVersion appVersion appName = do
    Logger.log "Checking if the repo is at the newest version..."
    repo        <- getRepo
    versionList <- Repo.getVersionsList repo appName
    if Prologue.null versionList then do
        Logger.log "> Yes"
        return True
    else do
        let newest  = (head versionList) < appVersion
        Logger.log $ if newest then "> Yes" else "> No"
        return newest

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

prepareVersion :: MonadCreatePackage m => FilePath -> Version -> m ()
prepareVersion appPath version = Shelly.switchVerbosity $ do
    -- check out to the commit pointed by the version tag, if it exists
    permitNoTagsFlag <- gets @PackageConfig permitNoTags
    let versionTxt  = showPretty version
        tagExists t = not . T.null <$> Shelly.cmd "git" "tag" "-l" t
    Shelly.chdir appPath $ do
        exists <- tagExists versionTxt
        Logger.log $ "Tag " <> versionTxt <> " " <> (if exists then "exists" else "does not exist.")
        if exists then do
            Logger.log "Checking out the tag..."
            Shelly.cmd "git" "checkout" versionTxt
        else if permitNoTagsFlag then Logger.log "[WARNING] No tag, building the package from head"
        else throwM $ NoTagException versionTxt

        commitHash <- Shelly.cmd "git" "rev-parse" "--short" "HEAD"
        Logger.log $ "Building from commit: " <> commitHash

createPkg :: MonadCreatePackage m => FilePath -> Maybe Text -> ResolvedApplication -> m ()
createPkg cfgFolderPath s3GuiURL resolvedApplication = do
    pkgConfig <- get @PackageConfig
    let app        = resolvedApplication ^. resolvedApp
        appDesc    = app ^. desc
        appPath    = if (appDesc ^. path) == "./" then cfgFolderPath else convert (appDesc ^. path)
        appHeader  = app ^. header
        appName    = appHeader ^. name
        appType    = app ^. resolvedAppType
        buildHead  = pkgConfig ^. buildFromHead
    appVersion <- do
        isNewest <- isNewestVersion (appHeader ^. version) appName
        if isNewest then return $ appHeader ^. version
                    else throwM $ ExistingVersionException (appHeader ^. version)

    Logger.log $ "Creating version: " <> (showPretty appVersion)

    mapM_ (downloadAndUnpackDependency appPath) $ resolvedApplication ^. pkgsToPack
    -- Save the current branch to return from the detached head state after switching to the tag
    currBranch <- Shelly.silently $ Shelly.chdir appPath $ Text.strip <$> Shelly.cmd "git" "rev-parse" "--abbrev-ref" "HEAD"
    unless buildHead $ prepareVersion appPath appVersion

    repo <- expand $ appPath
    let componentsFolder = pkgConfig ^. componentsToCopy
        versionFile = repo </> componentsFolder </> (pkgConfig ^. configFolder) </> (pkgConfig ^. versionFileName)

    Shelly.mkdir_p $ parent versionFile
    liftIO $ writeFile (encodeString versionFile) $ convert $ showPretty appVersion

    runPkgBuildScript appPath s3GuiURL
    copyFromDistToDistPkg appName appPath
    mainAppDir <- case currentHost of
        Windows -> return $ (pkgConfig ^. defaultPackagePath) </> convert appName
        _       -> expand $ appPath </> (pkgConfig ^. defaultPackagePath) </> convert appName
    let binsFolder  = mainAppDir </> (pkgConfig ^. binFolder) </> (pkgConfig ^. binsPrivate)
        libsFolder  = mainAppDir </> (pkgConfig ^. libPath)


    when (currentHost == Darwin) $ Shelly.silently $ linkLibs binsFolder libsFolder

    package <- case currentHost of
                  Linux -> createAppimage appName appVersion appPath
                  _     -> Archive.pack mainAppDir $ finalPackageName appName appVersion

    generateChecksum  @Crypto.SHA256 package

    unless buildHead $ Shelly.switchVerbosity $ Shelly.chdir appPath $ do
        Shelly.cmd "git" "checkout" currBranch

run :: MonadCreatePackage m => MakePackageOpts -> m ()
run opts = do
    guiInstaller <- guiInstallerOpt
    config       <- parseConfig $ convert (opts ^. Opts.cfgPath)
    modify_ @PackageConfig (permitNoTags  .~ (opts ^. Opts.permitNoTags))
    modify_ @PackageConfig (buildFromHead .~ (opts ^. Opts.buildFromHead))

    let cfgFolderPath = parent $ convert (opts ^. Opts.cfgPath)
        appsToPack    = config ^. apps
        s3GuiUrl      = opts ^. Opts.guiURL

    resolved <- mapM (resolvePackageApp config) appsToPack

    mapM_ (createPkg cfgFolderPath s3GuiUrl) resolved
    repo <- getRepo
    let updatedConfig = foldl' updateConfig config resolved
    generateConfigYamlWithNewPackage repo updatedConfig $ cfgFolderPath </> "config.yaml"
