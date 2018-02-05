{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
module Luna.Manager.Command.Version where

import           Prologue
import           Luna.Manager.Component.Version.TH (getVersion)


version :: String
version = $(getVersion)


run :: MonadIO m => m ()
run = liftIO . putStrLn $ "Luna Manager version " <> version
