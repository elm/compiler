{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Type
  ( Type(..)
  , Format(..)
  , toString
  , Program(..)
  , encode
  , decoder
  , encodeProgram
  )
  where

import Control.Arrow ((***))
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Text.PrettyPrint as P
import Text.PrettyPrint ((<+>))

import qualified AST.Helpers as Help
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Parse.Helpers as Parse
import qualified Parse.Type as Type
import qualified Reporting.Annotation as A
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode
import Json.Encode ((==>))



-- TYPES


data Type
    = Lambda Type Type
    | Var Text
    | Type Text [Type]
    | Record [(Text, Type)] (Maybe Type)


data Program =
  Program
    { _message :: Type
    , _aliases :: [( Text, [Text], Type )]
    , _unions :: [( Text, [Text], [(Text, [Type])] )]
    }



-- TO STRING


data Format = OneLine | MultiLine


toString :: Format -> Type -> String
toString format tipe =
  let
    mode =
      case format of
        OneLine ->
          P.OneLineMode
        MultiLine ->
          P.PageMode
  in
    P.renderStyle (P.Style mode 80 1.0) (toDoc None tipe)


data Context = None | InType | InFunction


toDoc :: Context -> Type -> P.Doc
toDoc context tipe =
  case tipe of
    Lambda _ _ ->
        let t:ts =
              map (toDoc InFunction) (collectLambdas tipe)

            lambda =
              P.sep [ t, P.sep (map (P.text "->" <+>) ts) ]
        in
            case context of
              None -> lambda
              _ -> P.parens lambda

    Var name ->
        P.text (Text.unpack name)

    Type name [] ->
        P.text (if name == Help.zeroTuple then "()" else Text.unpack name)

    Type name arguments@(arg:args) ->
        if Help.isTuple name then
          P.sep
            [ P.cat ("(" <+> toDoc None arg : map (\a -> "," <+> toDoc None a) args)
            , ")"
            ]

        else
          let
            application =
              P.hang
                  (P.text (Text.unpack name))
                  2
                  (P.sep $ map (toDoc InType) arguments)
          in
            case context of
              InType ->
                P.parens application

              _ ->
                application

    Record _ _ ->
        case flattenRecord tipe of
            ([], Nothing) ->
                P.text "{}"

            (fields, Nothing) ->
                P.sep
                  [ P.cat (zipWith (<+>) ("{" : repeat ",") (map prettyField fields))
                  , "}"
                  ]

            (fields, Just x) ->
                P.hang
                    ("{" <+> P.text (Text.unpack x) <+> "|")
                    4
                    (P.sep
                      [ P.sep (P.punctuate "," (map prettyField fields))
                      , "}"
                      ]
                    )
          where
            prettyField (field, fieldType) =
                P.text (Text.unpack field) <+> ":" <+> toDoc None fieldType


collectLambdas :: Type -> [Type]
collectLambdas tipe =
  case tipe of
    Lambda arg body -> arg : collectLambdas body
    _ -> [tipe]


flattenRecord :: Type -> ( [(Text, Type)], Maybe Text )
flattenRecord tipe =
  case tipe of
    Var x ->
      ([], Just x)

    Record fields Nothing ->
      (fields, Nothing)

    Record fields (Just ext) ->
      let (fields',ext') = flattenRecord ext
      in
          (fields' ++ fields, ext')

    _ ->
      error "Trying to flatten ill-formed record."



-- JSON for TYPE


encode :: Type -> Encode.Value
encode tipe =
  Encode.text (Text.pack (toString OneLine tipe))


decoder :: Decode.Decoder Type
decoder =
  Decode.oneOf
    [
      do  txt <- Decode.text
          case Parse.run Type.expression (Text.replace "'" "_" txt) of
            Left _ ->
              Decode.fail "a type, like (String -> Int)"

            Right (tipe, _, _) ->
              Decode.succeed (fromRawType tipe)
    ,
      Decode.aeson
    ]


instance Aeson.FromJSON Type where
  parseJSON =
    Aeson.withObject "Type" $ \object ->
      do  tag <- object .: "tag"
          case (tag :: String) of
            "lambda" ->
                Lambda <$> object .: "in" <*> object .: "out"

            "var" ->
                Var <$> object .: "name"

            "type" ->
                do  name <- object .: "name"
                    return (Type name [])

            "app" ->
                do  func <- object .: "func"
                    args <- object .: "args"
                    case func of
                      Type name [] ->
                        return (Type name args)

                      _ ->
                        fail "unexpected type application"

            "record" ->
                Record <$> object .: "fields" <*> object .: "extension"

            _ ->
                fail $ "Error when decoding type with tag: " ++ tag


fromRawType :: Type.Raw -> Type
fromRawType (A.A _ astType) =
  case astType of
    Type.RLambda t1 t2 ->
        Lambda (fromRawType t1) (fromRawType t2)

    Type.RVar x ->
        Var x

    Type.RType (A.A _ name) args ->
        Type (Var.rawToText name) (map fromRawType args)

    Type.RRecord fields ext ->
        Record (map (A.drop *** fromRawType) fields) (fmap fromRawType ext)



-- JSON for PROGRAM


encodeProgram :: Program -> Encode.Value
encodeProgram (Program msg aliases unions) =
  Encode.object
    [ "message" ==> encode msg
    , "aliases" ==> Encode.object (map toAliasField aliases)
    , "unions" ==> Encode.object (map toUnionField unions)
    ]


toAliasField :: ( Text, [Text], Type ) -> ( String, Encode.Value )
toAliasField ( name, args, tipe ) =
  Text.unpack name ==>
    Encode.object
      [ "args" ==> Encode.list Encode.text args
      , "type" ==> encode tipe
      ]


toUnionField :: ( Text, [Text], [(Text, [Type])] ) -> ( String, Encode.Value )
toUnionField ( name, args, constructors ) =
  Text.unpack name ==>
    Encode.object
      [ "args" ==> Encode.list Encode.text args
      , "tags" ==> Encode.object (map toCtorObject constructors)
      ]


toCtorObject :: (Text, [Type]) -> ( String, Encode.Value )
toCtorObject (name, args) =
  Text.unpack name ==> Encode.list encode args
