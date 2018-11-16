module Luna.Manager.Component.WindowsResource where

import Prologue hiding (FilePath, (<.>))

import Control.Monad.State.Layered
import Filesystem.Path.CurrentOS      (FilePath, decodeString,
                                       encodeString, filename,
                                       (</>))
import Luna.Manager.Command.Options   (Options)
import Luna.Manager.Component.Pretty
import Luna.Manager.Component.Version (Version(..), VersionInfo(..))
import Luna.Manager.Shell.Shelly      (runProcess, runRawSystem)
import Luna.Manager.System.Env
import System.Directory               (listDirectory)
import System.Environment             (getEnv)

import qualified Data.Text                              as Text
import qualified Data.Text.IO                           as Text
import qualified Filesystem.Path.CurrentOS              as FP
import qualified Luna.Manager.Legal                     as Legal
import qualified Luna.Manager.Shell.Shelly              as Shelly


type ResourceContext m =
    ( MonadGetter Options m
    , MonadStates '[EnvConfig] m
    , Shelly.MonadSh m
    , Shelly.MonadShControl m
    , MonadCatch m
    , MonadIO m
    )

updateExeInfo :: ResourceContext m
    => Version -> FilePath -> m ()
updateExeInfo version exePath = do
    let exeName    = filename exePath
        resHacker  =
            "C:\\Program Files (x86)\\Resource Hacker\\ResourceHacker.exe"
        resPath    = FP.replaceExtension exePath "res"
        rcPath     = FP.replaceExtension exePath "rc"
    liftIO $ Text.writeFile (encodeString rcPath) $
        createExeVersionManifest version (convert $ encodeString exeName)
    runRawSystem resHacker [ "-open", Shelly.toTextIgnore rcPath
                           , "-save", Shelly.toTextIgnore resPath
                           , "-action", "compile"
                           , "-log", "CONSOLE"
                           ]
    runRawSystem resHacker [ "-open", Shelly.toTextIgnore exePath
                           , "-save", Shelly.toTextIgnore exePath
                           , "-action", "addoverwrite"
                           , "-resource", Shelly.toTextIgnore resPath
                           , "-log", "CONSOLE"
                           ]
    Shelly.rm_rf rcPath
    Shelly.rm_rf resPath

createExeVersionManifest :: Version -> Text -> Text
createExeVersionManifest version exeName =
    let fullVersion (Version maj min vi) = Version maj min $ Just $ case vi of
            Nothing                -> VersionInfo 0 (Just 0)
            Just (VersionInfo n b) -> VersionInfo n (Just $ fromMaybe 0 b)
        versionFormat    = showPretty $ fullVersion version
        winVersionFormat = Text.replace "." "," versionFormat
    in Text.unlines [
            "VS_VERSION_INFO VERSIONINFO"
          , Text.concat ["    FILEVERSION    ", winVersionFormat]
          , Text.concat ["    PRODUCTVERSION ", winVersionFormat]
          , "{"
          , "    BLOCK \"StringFileInfo\""
          , "    {"
          , "        BLOCK \"040904b0\""
          , "        {"
          , Text.concat [
              "            VALUE \"CompanyName\",        \""
            , Legal.companyName
            , "\""
            ]
          , Text.concat [
              "            VALUE \"FileDescription\",    \""
            , Legal.productDescription
            , "\""
            ]
          , Text.concat [
              "            VALUE \"FileVersion\",        \""
            , versionFormat
            , "\""
            ]
          , Text.concat [
              "            VALUE \"LegalCopyright\",     \""
            , Legal.copyright
            , "\""
            ]
          , Text.concat [
              "            VALUE \"OriginalFilename\",   \""
            , exeName
            , "\""
            ]
          , Text.concat [
              "            VALUE \"ProductName\",        \""
            , Legal.productName
            , "\""
            ]
          , Text.concat [
              "            VALUE \"ProductVersion\",     \""
            , versionFormat
            , "\""
            ]
          , "        }"
          , "    }"
          , "    BLOCK \"VarFileInfo\""
          , "    {"
          , "        VALUE \"Translation\", 0x409, 1200"
          , "    }"
          , "}"
        ]


updateWindowsMetadata :: ResourceContext m
    => Version -> FilePath -> FilePath -> m ()
updateWindowsMetadata version privateBinFolder mainBin = do
    updateExeInfo version mainBin
    binaries <- liftIO . listDirectory $ encodeString privateBinFolder
    forM_ binaries $ \b -> do
        let fullPath = privateBinFolder </> decodeString b
        updateExeInfo version fullPath


signWindowsBinary :: ResourceContext m => Text -> m ()
signWindowsBinary binPath = do
    let signTool     = "c:\\Program Files (x86)\\Windows Kits\\10\\bin\\x64\\signtool.exe"
        timestampUrl = "http://timestamp.digicert.com"
    certPath <- liftIO $ convert <$> getEnv "CERT_PATH"
    certPass <- liftIO $ convert <$> getEnv "CERT_PASS"
    runProcess signTool [ "sign", "/v", "/f", certPath, "/p", certPass
                        , "/t", timestampUrl, binPath
                        ]

signWindowsBinaries :: ResourceContext m
                    => FilePath -> FilePath -> m ()
signWindowsBinaries privateBinFolder mainBin = do
    signWindowsBinary $ Shelly.toTextIgnore mainBin
    binaries <- liftIO . listDirectory $ encodeString privateBinFolder
    forM_ binaries $ \b -> do
        let fullPath  = privateBinFolder </> decodeString b
        signWindowsBinary $ Shelly.toTextIgnore fullPath
