{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Type
  ( Type(..), toString
  , Program(..)
  , encode
  , decoder
  , encodeProgram
  )
  where

import Control.Arrow ((***))
import qualified Data.Text as Text
import Data.Text (Text)
import Text.PrettyPrint as P

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
    | Type Text
    | App Type [Type]
    | Record [(Text, Type)] (Maybe Type)


data Program =
  Program
    { _message :: Type
    , _aliases :: [( Text, [Text], Type )]
    , _unions :: [( Text, [Text], [(Text, [Type])] )]
    }



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
        P.text (Text.unpack name)

    Type name ->
        P.text (Text.unpack (if name == "_Tuple0" then "()" else name))

    App (Type name) args ->
        if name == "_Tuple0" then
          P.text "()"

        else if Help.isTuple name then
          P.sep
            [ P.cat (zipWith (<+>) (P.lparen : repeat P.comma) (map (toDoc None) args))
            , P.rparen
            ]

        else
          let
              application =
                  P.hang
                      (P.text (Text.unpack name))
                      2
                      (P.sep $ map (toDoc ADT) args)
          in
              case (context, args) of
                (ADT, _ : _) -> P.parens application
                _ -> application

    App _ _ ->
        error $
          "Somehow ended up with an unexpected type, please post a minimal example that\n"
          ++ "reproduces this error to <https://github.com/elm-lang/elm-compiler/issues>"

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
                    (P.lbrace <+> P.text (Text.unpack x) <+> P.text "|")
                    4
                    (P.sep
                      [ P.sep (P.punctuate P.comma (map prettyField fields))
                      , P.rbrace
                      ]
                    )
          where
            prettyField (field, fieldType) =
                P.text (Text.unpack field) <+> P.text ":" <+> toDoc None fieldType


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
  Encode.text (Text.unwords (Text.words (Text.pack (toString tipe))))


decoder :: Decode.Decoder Type
decoder =
  do  txt <- Decode.text
      case Parse.run Type.expression txt of
        Left _ ->
          Decode.fail "a type, like (String -> Int)"

        Right (tipe, _, _) ->
          Decode.succeed (fromRawType tipe)


fromRawType :: Type.Raw -> Type
fromRawType (A.A _ astType) =
  case astType of
    Type.RLambda t1 t2 ->
        Lambda (fromRawType t1) (fromRawType t2)

    Type.RVar x ->
        Var x

    Type.RType var ->
        Type (Var.rawToText var)

    Type.RApp t ts ->
        App (fromRawType t) (map fromRawType ts)

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
