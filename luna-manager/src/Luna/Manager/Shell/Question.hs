module Luna.Manager.Shell.Question where

import Prologue hiding (txt)

import Control.Monad.Exception (MonadException, throw)
import System.IO               (hFlush, stdout)

import Luna.Manager.Component.Pretty


-------------------
-- === Utils === --
-------------------
-- FIXME: We should prbably refactor it out in some time

listItem :: Text -> Text
listItem n = "  - " <> n

listItems :: [Text] -> Text
listItems = intercalate "\n" . fmap listItem



-----------------------
-- === Questions === --
-----------------------

-- === Definition === --

type ArgReader a = Text -> Either Text a
data Question  a = Question { _txt    :: Text
                            , _reader :: ArgReader a
                            , _help   :: Maybe Text
                            , _defArg :: Maybe Text
                            }

makeLenses ''Question


-- === Errors === --

data InvalidArgError = InvalidArgError deriving (Show)
instance Exception InvalidArgError where
    displayException _ = "Invalid argument provided."

invalidArgError :: SomeException
invalidArgError = toException InvalidArgError


-- === Utils === --

question :: Text -> ArgReader a -> Question a
question t r = Question t r mempty mempty

choiceValidator  :: Text -> Text -> Maybe (Either Text a) -> Either Text a
choiceValidator' :: Text -> Text -> Maybe a               -> Either Text a
choiceValidator  s t = fromMaybe (Left $ "Unknown " <> s <> " '" <> t <> "' selected.")
choiceValidator' s t = choiceValidator s t . fmap Right

choiceHelp :: Pretty a => Text -> [a] -> Maybe Text
choiceHelp s ts = Just $ "Available " <> s <> ":\n" <> listItems (showPretty <$> ts)

plainTextReader :: ArgReader Text
plainTextReader = Right

-- === Running === --

askOrUse :: (MonadIO m, MonadException SomeException m) => Maybe Text -> Question a -> m a
askOrUse mdef q = case mdef of
    Nothing -> ask q
    Just s  -> validate (q ^. reader) (throw invalidArgError) s

ask :: MonadIO m => Question a -> m a
ask q  = validate (q ^. reader) (ask q) =<< askRaw q

askRaw :: MonadIO m => Question a -> m Text
askRaw q = do
    let defAns       = maybe "" (\s -> " [" <> s <> "]") (q ^. defArg)
        questionLine = q ^. txt <> defAns <> ": "
        printHeader  = do
            putStrLn ""
            mapM (putStrLn . convert) (q ^. help)
        goQuestion   = do
            resp <- askLine
            if resp /= "" then pure resp
                else maybe goQuestion pure (q ^. defArg)
        askLine      = do
            putStr $ convert questionLine
            liftIO $ hFlush stdout
            liftIO $ convert <$> getLine
    printHeader
    goQuestion

validate :: MonadIO m => ArgReader a -> m a -> Text -> m a
validate reader f resp = case reader resp of
    Left  e -> putStrLn ("Error: " <> convert e) >> f
    Right a -> pure a
