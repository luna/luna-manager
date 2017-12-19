{-# LANGUAGE OverloadedStrings #-}

module Luna.Manager.System where

import           Prologue                     hiding (FilePath,null, filter, appendFile, readFile, toText, fromText)

import           Control.Monad.Raise
import           Control.Monad.State.Layered
import           Control.Monad.Trans.Resource (MonadBaseControl)
import           Data.ByteString.Lazy         (ByteString, null)
import           Data.ByteString.Lazy.Char8   (filter)
import           Data.Maybe                   (listToMaybe)
import           Data.List.Split              (splitOn)
import           Data.Text.IO                 (appendFile, readFile)
import qualified Data.Text                    as Text
import           Filesystem.Path.CurrentOS    (FilePath, (</>), encodeString, toText, parent)
import           System.Directory             (executable, setPermissions, getPermissions, doesPathExist, getHomeDirectory)
import qualified System.Environment           as Environment
import           System.Exit
import           System.Process.Typed         as Process

import           Luna.Manager.Command.Options (Options)
import qualified Luna.Manager.Logger          as Logger
import           Luna.Manager.Logger          (LoggerMonad)
import qualified Luna.Manager.Shell.Shelly    as Shelly
import           Luna.Manager.Shell.Shelly    (MonadSh, MonadShControl)
import           Luna.Manager.System.Env
import           Luna.Manager.System.Host


data Shell = Bash | Zsh | Unknown deriving (Show)

isEnter :: Char -> Bool
isEnter a = a /= '\n'

filterEnters :: ByteString ->   ByteString
filterEnters bytestr = res where
    res = filter isEnter bytestr

callShell :: MonadIO m => Text -> m ByteString
callShell cmd = do
    (exitCode, out, err) <- liftIO $ readProcess (shell $ convert cmd)
    return $ filterEnters out

checkIfNotEmpty :: MonadIO m => Text -> m Bool
checkIfNotEmpty cmd = not . null <$> callShell cmd

checkBash :: MonadIO m => m  (Maybe Shell)
checkBash = do
    sys <- checkIfNotEmpty "$SHELL -c 'echo $BASH_VERSION'"
    return $ if sys then Just Bash else Nothing

checkZsh :: MonadIO m => m  (Maybe Shell)
checkZsh = do
    sys <- checkIfNotEmpty "$SHELL -c 'echo $ZSH_VERSION'"
    return $ if sys then Just Zsh else Nothing

checkShell :: MonadIO m => m Shell
checkShell = do
    bash <- checkBash
    zsh  <- checkZsh
    return $ fromMaybe Unknown $ bash <|> zsh

runControlCheck :: MonadIO m => FilePath -> m (Maybe FilePath)
runControlCheck file = do
    home <- getHomePath
    let location = home </> file
    pathCheck <- liftIO $ doesPathExist $ encodeString location
    return $ if pathCheck then Just location else Nothing

data BashConfigNotFoundError = BashConfigNotFoundError deriving (Show)
instance Exception BashConfigNotFoundError where
    displayException _ = "Bash config not found"

bashConfigNotFoundError :: SomeException
bashConfigNotFoundError = toException BashConfigNotFoundError

data UnrecognizedShellException = UnrecognizedShellException deriving (Show)
instance Exception UnrecognizedShellException where
    displayException _ = "Unrecognized shell. Please add ~/.local/bin to your exports."

unrecognizedShellError :: SomeException
unrecognizedShellError = toException UnrecognizedShellException

-- exportPath' :: (MonadIO m, LoggerMonad m) => FilePath -> m ()
-- exportPath' pathToExport = case currentHost of
--     Windows -> exportPathWindows pathToExport
--     _       -> exportPath (parent pathToExport)
    -- | e == unrecognizedShellError  -> error "d"
    -- | e == bashConfigNotFoundError -> error "x"
    -- UnrecognizedShellException ->
    -- `catches` [Handler (\ (ex :: UnrecognizedShellException) -> liftIO $ putStrLn $ displayException ex),
    --                                                                               Handler (\ (ex :: BashConfigNotFoundError) -> liftIO $ putStrLn $ displayException ex)]

getShExportFile :: MonadIO m => m (Maybe FilePath)
getShExportFile = do
    shellType <- checkShell
    let files = case shellType of
             Bash -> [".bashrc", ".bash_profile", ".profile"]
             Zsh  -> [".zshrc",  ".zprofile",     ".profile"]
             _    -> [".profile"]
    checkedFiles <- mapM runControlCheck files
    return $ listToMaybe $ catMaybes checkedFiles

--TODO wyextrachowac wspolna logike dla poszczególnych terminali
exportPathUnix :: (MonadIO m, MonadBaseControl IO m, LoggerMonad m) => FilePath -> m ()
exportPathUnix pathToExport = do
    file <- getShExportFile
    let pathToExportText = convert $ encodeString pathToExport
        exportToAppend   = Text.concat ["\nexport PATH=", pathToExportText, ":$PATH\n"]
        warn             = Logger.warning "Unable to export Luna path. Please add ~/.local/bin to your PATH"
    case file of
        Just f  -> do
            let path = encodeString f
            exportExists <- Text.isInfixOf exportToAppend <$> liftIO (readFile path)
            unless exportExists $
                (liftIO $ appendFile path exportToAppend) `Shelly.catchany` (const warn)
        Nothing -> warn

exportPathWindows :: MonadIO m => FilePath -> m ()
exportPathWindows path = liftIO $ do
    (exitCode, out, err) <- Process.readProcess $ Process.shell $ "setx Path \"" ++ (encodeString $ parent path) ++ "\""
    unless (exitCode == ExitSuccess) $ print $ "Path was not exported." <> err  -- TODO this should be warning not print but installation was succesfull just path was not exported

makeExecutable :: MonadIO m => FilePath -> m ()
makeExecutable file = unless (currentHost == Windows) $ liftIO $ do
        p <- getPermissions $ encodeString file
        setPermissions (encodeString file) (p {executable = True})

runServicesWindows :: (MonadSh m, MonadIO m, MonadShControl m) => FilePath -> FilePath -> m ()
runServicesWindows path logsPath = Shelly.chdir path $ do
    Shelly.mkdir_p logsPath
    let installPath = path </> Shelly.fromText "installAll.bat"
    Shelly.setenv "LOGSDIR" $ Shelly.toTextIgnore logsPath
    Shelly.silently $ Shelly.cmd installPath --TODO create proper error

stopServicesWindows :: MonadIO m => FilePath -> m ()
stopServicesWindows path = Shelly.shelly $ Shelly.chdir path $ do
        let uninstallPath = path </> Shelly.fromText "uninstallAll.bat"
        Shelly.silently $ Shelly.cmd uninstallPath `catch` handler where
            handler :: MonadSh m => SomeException -> m ()
            handler ex = return () -- Shelly.liftSh $ print ex --TODO create proper error
