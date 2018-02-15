module Luna.Manager.Shell.ProgressBar where

import Prelude

import GHC.Generics
import Control.Monad.IO.Class
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)

import Data.Conduit (ConduitM, await, yield, ($$+-),($=+))
import Data.List (genericReplicate)
import Data.Ratio ( (%) )
import qualified Data.Text as Text

import Luna.Manager.Gui.DownloadProgress

import Text.Printf   ( printf )
import System.Console.ANSI (clearLine, cursorUpLine)

-- === Definition === --

data ProgressBar = ProgressBar { barWidth  :: Int --total progress bar width
                               , completed :: Int --Amount of work completed
                               , totalWork :: Int --total amount of work
                               }

--------------------------------
------ ProgressBarUtils --------
--------------------------------

progressBar :: MonadIO m => ProgressBar -> m ()
progressBar (ProgressBar width todo done) = liftIO $ do
    putStrLn $ printf "[%s%s%s]" (genericReplicate completed ('=' :: Char)) (if remaining /= 0 && completed /= 0 then (">" :: String) else "") (genericReplicate (remaining - if completed /= 0 then 1 else 0) '.')
    cursorUpLine 1
    clearLine
    where
        fraction :: Rational
        fraction | done /= 0  = fromIntegral todo % fromIntegral done
                 | otherwise = 0 % 1

        effectiveWidth = max 0 $ width - usedSpace
        usedSpace = 2

        numCompletedChars :: Rational
        numCompletedChars = fraction * (fromIntegral effectiveWidth % 1)

        completed, remaining :: Int
        completed = min effectiveWidth $ floor numCompletedChars
        remaining = effectiveWidth - completed

updateProgress :: MonadIO m => Progress -> ConduitM ByteString ByteString m ()
updateProgress (Progress completed total) = await >>= maybe (return ()) (\chunk -> do
    let len = ByteString.length chunk
        pg = Progress (completed+len) total
    liftIO $ downloadProgress pg
    yield chunk
    updateProgress pg)

updateProgressBar :: MonadIO m => ProgressBar -> ConduitM ByteString ByteString m ()
updateProgressBar (ProgressBar w completed pgTotal) = await >>= maybe (return ()) (\chunk -> do
    let len = ByteString.length chunk
        pg = ProgressBar w (completed+len) pgTotal
    liftIO $ progressBar pg
    yield chunk
    updateProgressBar pg)
