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
import qualified Data.Text as Text
import Text.PrettyPrint as P

import qualified AST.Helpers as Help


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
  P.render (toDoc None tipe)


toDoc :: Context -> Type -> P.Doc
toDoc context tipe =
  case tipe of
    Lambda _ _ ->
        let t:ts =
              map (toDoc Function) (collectLambdas tipe)

            lambda =
              P.sep [ t, P.sep (map (P.text "->" <+>) ts) ]
        in
            case context of
              None -> lambda
              _ -> P.parens lambda

    Var name ->
        P.text name

    Type name ->
        P.text (if name == "_Tuple0" then "()" else name)

    App (Type name) args
        | Help.isTuple name ->
            P.sep
              [ P.cat (zipWith (<+>) (P.lparen : repeat P.comma) (map (toDoc None) args))
              , P.rparen
              ]

        | otherwise ->
            let adt = P.hang (P.text name) 2 (P.sep $ map (toDoc ADT) args)
            in
                case (context, args) of
                  (ADT, _ : _) -> P.parens adt
                  _ -> adt

    Record _ _ ->
        case flattenRecord tipe of
            ([], Nothing) ->
                P.text "{}"

            (fields, Nothing) ->
                P.sep
                  [ P.cat (zipWith (<+>) (P.lbrace : repeat P.comma) (map prettyField fields))
                  , P.rbrace
                  ]

            (fields, Just x) ->
                P.hang
                    (P.lbrace <+> P.text x <+> P.text "|")
                    4
                    (P.sep
                      [ P.cat (zipWith (<+>) (P.space : repeat P.comma) (map prettyField fields))
                      , P.rbrace
                      ])
          where
            prettyField (field, tipe) =
                P.text field <+> P.text ":" <+> toDoc None tipe


collectLambdas :: Type -> [Type]
collectLambdas tipe =
  case tipe of
    Lambda arg body -> arg : collectLambdas body
    _ -> [tipe]


flattenRecord :: Type -> ( [(String, Type)], Maybe String )
flattenRecord tipe =
    case tipe of
      Var x -> ([], Just x)

      Record fields Nothing -> (fields, Nothing)

      Record fields (Just ext) ->
          let (fields',ext') = flattenRecord ext
          in
              (fields' ++ fields, ext')

      _ -> error "Trying to flatten ill-formed record."


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
