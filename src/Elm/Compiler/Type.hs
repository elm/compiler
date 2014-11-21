{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Type
    ( Type(..)
    , toString
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.List as List
import qualified Data.Text as Text

import qualified AST.Helpers as Help
import Elm.Utils ((|>))


data Type
    = Lambda Type Type
    | Var String
    | Type String
    | App Type [Type]
    | Record [(String, Type)] (Maybe Type)


-- TO STRING

data Context = None | ADT | Function


toString :: Type -> String
toString tipe =
  toStringHelp None tipe


toStringHelp :: Context -> Type -> String
toStringHelp context tipe =
  case tipe of
    Lambda t1 t2 ->
        let string = toStringHelp Function t1 ++ " -> " ++ toString t2
        in
            case context of
              None -> string
              _ -> parens string

    Var name -> name

    Type name -> name

    App (Type "List") [t] ->
        sandwich "[" "]" (toString t)

    App (Type name) args
        | Help.isTuple name ->
            map toString args
                |> List.intercalate ", "
                |> parens

        | otherwise ->
            let string = name ++ spacePrefix (map (toStringHelp ADT) args)
            in
                case (context, args) of
                  (ADT, _ : _) -> parens string
                  _ -> string

    Record fields maybeExtension ->
        let viewField (key, value) =
                key ++ " : " ++ toString value

            viewExtension maybeType =
                case maybeType of
                  Nothing -> ""
                  Just t -> toString t ++ " | "
        in
            sandwich "{ " " }" $
              concat
              [ viewExtension maybeExtension
              , map viewField fields
                  |> List.intercalate ", "
              ]


parens :: String -> String
parens string =
  sandwich "(" ")" string


spacePrefix :: [String] -> String
spacePrefix strings =
  concatMap (" " ++) strings


sandwich :: String -> String -> String -> String
sandwich start stop string =
    start ++ string ++ stop


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
