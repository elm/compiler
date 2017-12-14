{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Type
  ( Type(..)
  , Format(..)
  , toString
  , DebugMetadata(..)
  , Alias(..)
  , Union(..)
  , encode
  , decoder
  , encodeMetadata
  )
  where


import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.PrettyPrint as P
import Text.PrettyPrint ((<+>))

import qualified AST.Source as Src
import qualified Elm.Name as N
import qualified Parse.Primitives as Parse
import qualified Parse.Type as Type
import qualified Reporting.Annotation as A
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode
import Json.Encode ((==>))



-- TYPES


data Type
    = Lambda Type Type
    | Var N.Name
    | Type N.Name [Type]
    | Record [(N.Name, Type)] (Maybe Type)
    | Unit
    | Tuple Type Type [Type]


data DebugMetadata =
  DebugMetadata
    { _message :: Type
    , _aliases :: [Alias]
    , _unions :: [Union]
    }


data Alias = Alias N.Name [N.Name] Type
data Union = Union N.Name [N.Name] [(N.Name, [Type])]



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

    Unit ->
        "()"

    Tuple a b cs ->
        P.sep
          [ P.cat $
              [ "(" <+> toDoc None a
              , "," <+> toDoc None b
              ]
              ++ map ("," <+>) (map (toDoc None) cs)
          , ")"
          ]

    Type name args ->
        case args of
          [] ->
            P.text (Text.unpack name)

          _ ->
            let
              application =
                P.hang
                    (P.text (Text.unpack name))
                    2
                    (P.sep $ map (toDoc InType) args)
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
  do  txt <- Decode.text
      case Parse.run Type.expression (Text.encodeUtf8 (Text.replace "'" "_" txt)) of
        Left _ ->
          Decode.fail "Expecting a type, like (String -> Int)"

        Right (tipe, _, _) ->
          Decode.succeed (fromRawType tipe)


fromRawType :: Src.Type -> Type
fromRawType (A.At _ astType) =
  case astType of
    Src.TLambda t1 t2 ->
        Lambda (fromRawType t1) (fromRawType t2)

    Src.TVar x ->
        Var x

    Src.TUnit ->
        Unit

    Src.TTuple a b cs ->
        Tuple
          (fromRawType a)
          (fromRawType b)
          (map fromRawType cs)

    Src.TType _ _ name args ->
        Type name (map fromRawType args)

    Src.TRecord fields ext ->
        let fromField (A.At _ field, tipe) = (field, fromRawType tipe) in
        Record
          (map fromField fields)
          (fmap fromRawType ext)



-- JSON for PROGRAM


encodeMetadata :: DebugMetadata -> Encode.Value
encodeMetadata (DebugMetadata msg aliases unions) =
  Encode.object
    [ "message" ==> encode msg
    , "aliases" ==> Encode.object (map toAliasField aliases)
    , "unions" ==> Encode.object (map toUnionField unions)
    ]


toAliasField :: Alias -> ( String, Encode.Value )
toAliasField (Alias name args tipe) =
  Text.unpack name ==>
    Encode.object
      [ "args" ==> Encode.list Encode.text args
      , "type" ==> encode tipe
      ]


toUnionField :: Union -> ( String, Encode.Value )
toUnionField (Union name args constructors) =
  Text.unpack name ==>
    Encode.object
      [ "args" ==> Encode.list Encode.text args
      , "tags" ==> Encode.object (map toCtorObject constructors)
      ]


toCtorObject :: (Text, [Type]) -> ( String, Encode.Value )
toCtorObject (name, args) =
  Text.unpack name ==> Encode.list encode args
