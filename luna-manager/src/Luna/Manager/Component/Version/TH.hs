{-# LANGUAGE TemplateHaskell #-}
module Luna.Manager.Component.Version.TH (getVersion) where

import Prologue hiding (lift)

import qualified Data.HashMap.Strict as HM
import qualified Data.Yaml           as Yaml

import Control.Lens    ((^?!), _Right)
import Data.ByteString (ByteString)

import Data.FileEmbed
import Language.Haskell.TH.Syntax


getVersionEither :: ByteString -> Either String Text
getVersionEither bs = decoded >>= getMap >>= getVer >>= getText
    where decoded   = Yaml.decodeEither bs :: Either String Yaml.Value
          err msg   = Left $ "Malformed Yaml (" <> msg <> ")"
          getMap  x = case x of Yaml.Object o              -> Right o; _ -> err "not an Object"
          getVer  x = case HM.lookup "version" x of Just x -> Right x; _ -> err "no version"
          getText x = case x of Yaml.String s              -> Right s; _ -> err "version not a String"


packageYaml :: ByteString
packageYaml = $(embedFile "package.yaml")


fromRight' :: Either l r -> r
fromRight' x = x^?!_Right


unsafeGetVersion :: ByteString -> String
unsafeGetVersion = convert . fromRight' . getVersionEither


thUnsafeGetVersion :: ByteString -> Q Exp
thUnsafeGetVersion = lift . unsafeGetVersion


-- Utility function used by `Luna.Manager.Command.Version` that bakes in the
-- version info at compile time.
getVersion :: Q Exp
getVersion = thUnsafeGetVersion packageYaml
