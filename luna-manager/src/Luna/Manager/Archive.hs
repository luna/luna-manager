{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Luna.Manager.Archive where

import Prologue hiding (FilePath, (<.>))

import qualified Control.Exception.Safe         as Exception
import           Control.Monad.Raise
import           Control.Monad.State.Layered
import qualified Data.ByteString.Lazy           as BSL
import qualified Data.ByteString.Lazy.Char8     as BSLChar
import           Data.Either                    (either)
import           Data.IORef
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import qualified Data.Text.Read                 as Text
import           Filesystem.Path.CurrentOS      (FilePath, basename, directory,
                                                 encodeString, extension,
                                                 filename, parent, (<.>), (</>))
import qualified Filesystem.Path.CurrentOS      as FP
import           Luna.Manager.Command.Options   (Options)
import qualified Luna.Manager.Command.Options   as Opts
import qualified Luna.Manager.Logger            as Logger
import           Luna.Manager.Network
import           Luna.Manager.Shell.ProgressBar
import           Luna.Manager.Shell.Shelly      (MonadSh, runProcess)
import qualified Luna.Manager.Shell.Shelly      as Shelly
import           Luna.Manager.System.Host
import           System.Exit
import qualified System.Process.Typed           as Process
default (Text.Text)

type UnpackContext m = (MonadGetter Options m, MonadNetwork m, MonadSh m, Shelly.MonadShControl m, MonadIO m, MonadException SomeException m, MonadThrow m, MonadCatch m)

plainTextPath :: FilePath -> Text
plainTextPath = either id id . FP.toText

data ExtensionError = ExtensionError { exPath :: FilePath } deriving (Show)
instance Exception ExtensionError where
    displayException (ExtensionError p) = "ExtensionError: cannot get extension from path " <> (Text.unpack $ plainTextPath p)

extensionError :: FilePath -> SomeException
extensionError = toException . ExtensionError

data ProgressException = ProgressException String deriving (Show)
instance Exception ProgressException where
    displayException (ProgressException err) = "Can not return progress. " <> err

data UnpackingException = UnpackingException Text SomeException deriving (Show)
instance Exception UnpackingException where
    displayException (UnpackingException file exception) = "Archive cannot be unpacked: " <> convert file <> " because of: " <> displayException exception

unpackingException :: Text -> SomeException -> SomeException
unpackingException t e = toException $ UnpackingException t e

unpack :: UnpackContext m => Double -> Text.Text -> FilePath -> m FilePath
unpack totalProgress progressFieldName file = do
    Logger.info $ "Unpacking archive: " <> plainTextPath file
    ext          <- tryJust (extensionError file) $ extension file
    case currentHost of
        Windows -> unSevenZzipWin totalProgress progressFieldName file
        Darwin  -> case ext of
            "gz"  -> unpackTarGzUnix totalProgress progressFieldName file
            "zip" -> unzipUnix file
        Linux   -> case ext of
            "AppImage" -> return file
            "gz"       -> unpackTarGzUnix totalProgress progressFieldName file
            "rpm"      -> do
                let name = basename file
                    dir = directory file
                    fullFilename = filename file
                Shelly.mkdir_p $ dir </> name
                Shelly.cp_r file $ dir </> name
                unpackRPM (dir </> name </> fullFilename) (dir </> name)
                Shelly.rm $ dir </> name </> (filename file)
                return $ dir </> name

unzipUnix :: UnpackContext m => FilePath -> m FilePath
unzipUnix file = do
    let dir = directory file
        name = basename file
    Shelly.chdir dir $ do
        Shelly.mkdir_p name
        Shelly.cp file name
        Shelly.chdir (dir </> name) $ do
            out <- Shelly.switchVerbosity $ Shelly.cmd  "unzip" $ dir </> name </> filename file
            Shelly.rm $ dir </> name </> filename file
            listed <- Shelly.ls $ dir </> name
            if length listed == 1 then return $ head listed else return $ dir </> name

countingFilesLogger :: Text.Text -> Double -> IORef Int -> Int -> Text.Text -> IO ()
countingFilesLogger progressFieldName totalProgress lastNumber n t = do
    modifyIORef' lastNumber succ
    currentFileNumber <- readIORef lastNumber
    let progress = (fromIntegral  currentFileNumber / fromIntegral n :: Double) * totalProgress
    print $ "{\"" <> (convert progressFieldName) <> "\":\"" <> (show progress) <> "\"}"

directProgressLogger :: Text.Text -> Double ->Text.Text -> IO ()
directProgressLogger progressFieldName totalProgress actualProgress = do
    let parsedActualProgress = Text.rational actualProgress
    case parsedActualProgress of
        Right x -> do
            let progress = fst x * totalProgress
            print $ "{\"" <> (convert progressFieldName) <> "\":\"" <> (show progress) <> "\"}"
        Left err -> raise' $ ProgressException err

progressBarLogger :: Text.Text -> IO ()
progressBarLogger pg = do
    let parsedProgress = Text.rational pg
    case parsedProgress of
        Right x -> do
            let progress = ceiling $ fst x * (100 :: Double)
            progressBar $ ProgressBar 50 progress 100
        Left err -> raise' $ ProgressException err

unpackTarGzUnix :: UnpackContext m => Double -> Text.Text -> FilePath -> m FilePath
unpackTarGzUnix totalProgress progressFieldName file = do
    guiInstaller <- Opts.guiInstallerOpt
    let dir = directory file
        name = basename file
    Shelly.chdir dir $ do
        Shelly.mkdir_p name
        if guiInstaller then do
            (sysExit, stdout, stderr) <- Process.readProcess $ Process.shell $ "tar -tzf " <> (encodeString file) <> " | wc -l"
            let n = Text.decimal $ Text.strip $ Text.decodeUtf8 $ BSL.toStrict stdout
            case n of
                Right x -> do
                    currentUnpackingFileNumber <- liftIO $ newIORef 0
                    Shelly.log_stderr_with (countingFilesLogger progressFieldName totalProgress currentUnpackingFileNumber $ fst x)
                                         $ Shelly.cmd "tar" "-xvpzf" (Shelly.toTextIgnore file) "--strip=1" "-C" (Shelly.toTextIgnore name)
                Left err -> throwM (UnpackingException (Shelly.toTextIgnore file) (toException $ Exception.StringException err callStack ))
        else (Shelly.switchVerbosity $ Shelly.cmd  "tar" "-xpzf" file "--strip=1" "-C" name) `Exception.catchAny` (\err -> throwM (UnpackingException (Shelly.toTextIgnore file) $ toException err))
        listed <- Shelly.ls $ dir </> name
        if length listed == 1 then return $ head listed else return $ dir </> name

-- TODO: download unzipper if missing
unzipFileWindows :: UnpackContext m => FilePath -> m FilePath
unzipFileWindows zipFile = do
    let scriptPath = "http://packages.luna-lang.org/windows/j_unzip.vbs"
    script       <- downloadFromURL scriptPath "Downloading archiving tool"
    let dir = directory zipFile
        name = dir </> basename zipFile
    Shelly.switchVerbosity $ do
      Shelly.chdir dir $ do
          Shelly.mkdir_p name
          Shelly.cp zipFile name
          Shelly.cp script name
      Shelly.chdir (dir </> name) $ do
          Shelly.cmd "cscript" (filename script) (filename zipFile) `Exception.catchAny` (\err -> throwM (UnpackingException (Shelly.toTextIgnore zipFile) $ toException err))
          Shelly.rm $ dir </> name </> filename zipFile
          Shelly.rm $ dir </> name </> filename script
          listed <- Shelly.ls $ dir </> name
          return $ if length listed == 1 then head listed else dir </> name

untarWin :: UnpackContext m => Double -> Text.Text -> FilePath -> m FilePath
untarWin totalProgress progressFieldName zipFile = do
    let scriptPath = "http://packages.luna-lang.org/windows/tar2.exe"
    guiInstaller <- Opts.guiInstallerOpt
    script       <- downloadFromURL scriptPath "Downloading archiving tool"
    let dir = directory zipFile
        name = dir </> basename zipFile

    Shelly.mv script dir

    Shelly.chdir dir $ do
        Shelly.mkdir_p name
        if guiInstaller
            then Shelly.log_stdout_with (directProgressLogger progressFieldName totalProgress) $ Shelly.cmd (dir </> filename script) "untar" (filename zipFile) name
            else Shelly.log_stdout_with progressBarLogger $ Shelly.cmd (dir </> filename script) "untar" (filename zipFile) name `Exception.catchAny` (\err -> throwM (UnpackingException (Shelly.toTextIgnore zipFile) $ toException err))
        listed <- Shelly.ls $ dir </> name
        return $ if length listed == 1 then head listed else dir </> name

download7Zip :: UnpackContext m => m FilePath
download7Zip = do
    let scriptPath = "http://packages.luna-lang.org/windows/7z/7za.exe"
        dll1Path   = "http://packages.luna-lang.org/windows/7z/7za.dll"
        dll2Path   = "http://packages.luna-lang.org/windows/7z/7zxa.dll"

    guiInstaller <- Opts.guiInstallerOpt
    script       <- downloadFromURL scriptPath "Downloading archiving tool"
    _            <- downloadFromURL dll1Path   "Downloading the DLL-s (1)"
    _            <- downloadFromURL dll2Path   "Downloading the DLL-s (2)"

    return script

unSevenZzipWin :: UnpackContext m => Double -> Text.Text -> FilePath -> m FilePath
unSevenZzipWin totalProgress progressFieldName zipFile = do
    guiInstaller <- Opts.guiInstallerOpt
    script       <- download7Zip
    let dir      =  directory zipFile
        name     =  dir </> basename zipFile

    runProcess script [ "x", "-o" <> Shelly.toTextIgnore name
                      , "-y", Shelly.toTextIgnore zipFile
                      ]
    return name

pack :: UnpackContext m => FilePath -> Text -> m FilePath
pack = case currentHost of
    Windows -> sevenZipWindows
    _       -> gzipUnix

gzipWindows :: UnpackContext m => FilePath -> Text -> m FilePath
gzipWindows folder appName = do
    let name = parent folder </> Shelly.fromText (appName <> ".tar.gz")
    let scriptPath = "http://packages.luna-lang.org/windows/tar.exe"
    script <- downloadFromURL scriptPath "Downloading archiving tool"
    Shelly.chdir (parent folder) $ do
        Shelly.cp script $ parent folder
        Shelly.switchVerbosity $ Shelly.cmd (parent folder </> filename script) "tar" name folder
        return name

sevenZipWindows :: UnpackContext m => FilePath -> Text -> m FilePath
sevenZipWindows folder appName = do
    let dir = parent folder
    Shelly.chdir dir $ do
        script <- download7Zip
        let zipFileName = Shelly.fromText appName <.> "7z"
            filePattern = folder </> "*"
        Shelly.switchVerbosity $
            Shelly.cmd script "a" "-t7z" zipFileName filePattern
        return $ dir </> zipFileName

unpackRPM :: UnpackContext m => FilePath -> FilePath -> m ()
unpackRPM file filepath = liftIO $ do
    (exitCode, out, err) <- Process.readProcess $ Process.setWorkingDir (encodeString filepath) $ Process.shell $ "rpm2cpio " <> encodeString file <> " | cpio -idmv"
    unless (exitCode == ExitSuccess) $ throwM (UnpackingException (Shelly.toTextIgnore file) (toException $ Exception.StringException (BSLChar.unpack err) callStack )) -- print $ "Fatal: rpm not unpacked. " <> err

gzipUnix :: UnpackContext m => FilePath  -> Text -> m FilePath
gzipUnix folder appName = do
    let name =  parent folder </> Shelly.fromText (appName <> ".tar.gz")
    Shelly.chdir (parent folder) $ Shelly.switchVerbosity $ do
        Shelly.cmd "tar" "-cpzf" name $ filename folder
        return name
