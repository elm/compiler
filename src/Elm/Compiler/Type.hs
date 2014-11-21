{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Type
    ( Type(..)
    , toString
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as Text

import qualified AST.Type as Type
import qualified AST.Variable as Var


data Type
    = Lambda Type Type
    | Var String
    | Type String
    | App Type [Type]
    | Record [(String, Type)] (Maybe Type)


-- TO STRING

toString :: Type -> String
toString tipe =
    case tipe of
      Lambda t t' -> undefined
      Var x -> undefined
      Type name -> undefined
      App t ts -> undefined
      Record fields maybeExtension -> undefined


-- JSON for TYPE

instance Json.ToJSON Type where
    toJSON tipe =
        Json.object (getFields tipe)
      where
        getFields tipe =
            case tipe of
              Lambda t1 t2 ->
                  [ "tag" .= ("lambda" :: Text.Text)
                  , "in" .= Json.toJSON t1
                  , "out" .= Json.toJSON t2
                  ]

              Var x ->
                  [ "tag" .= ("var" :: Text.Text)
                  , "name" .= Json.toJSON x
                  ]

              Type name ->
                  [ "tag" .= ("type" :: Text.Text)
                  , "name" .= Json.toJSON name
                  ]

              App t ts -> 
                  [ "tag" .= ("app" :: Text.Text)
                  , "func" .= Json.toJSON t
                  , "args" .= Json.toJSON ts
                  ]

              Record fields ext ->
                  [ "tag" .= ("record" :: Text.Text)
                  , "fields" .= Json.toJSON (map (Json.toJSON . second Json.toJSON) fields)
                  , "extension" .= Json.toJSON ext
                  ]


instance Json.FromJSON Type where
    parseJSON (Json.Object obj) =
        do  tag <- obj .: "tag"
            case (tag :: String) of
              "lambda" ->
                  Lambda <$> obj .: "in" <*> obj .: "out"

              "var" ->
                  Var <$> obj .: "name"

              "type" ->
                  Type <$> obj .: "name"

              "app" ->
                  App <$> obj .: "func" <*> obj .: "args"

              "record" ->
                  Record <$> obj .: "fields" <*> obj .: "extension"

              _ ->
                  fail $ "Error when decoding type with tag: " ++ tag


    parseJSON value =
        fail $ "Cannot decode Value from: " ++ BS.unpack (Json.encode value)
