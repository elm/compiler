{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Docs
  ( Documentation(..)
  , Alias(..)
  , Union(..)
  , Value(..)
  , fromCheckedDocs
  , decoder
  , encode
  )
  where

import qualified Data.Map as Map
import Data.Text (Text)

import qualified Docs.AST as Docs
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Type as Type
import qualified Elm.Compiler.Version as Version
import qualified Elm.Package as Pkg
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode
import Json.Encode ((==>))



-- DATA STRUCTURES


data Documentation =
  Documentation
    { _name :: Module.Raw
    , _comment :: Text
    , _aliases :: [Alias]
    , _types :: [Union]
    , _values :: [Value]
    , _version :: Maybe Pkg.Version
    }


data Alias =
  Alias
    { _a_name :: Text
    , _a_comment :: Text
    , _a_args :: [Text]
    , _a_type :: Type.Type
    }


data Union =
  Union
    { _u_name :: Text
    , _u_comment :: Text
    , _u_args :: [Text]
    , _u_cases :: [(Text, [Type.Type])]
    }


data Value =
  Value
    { _v_name :: Text
    , _v_comment :: Text
    , _v_type :: Type.Type
    , _v_fix :: Maybe (Text,Int)
    }



-- FROM CHECKED DOCS


fromCheckedDocs :: Module.Raw -> Docs.Checked -> Documentation
fromCheckedDocs name (Docs.Checked comment values aliases unions) =
  let
    toValue (vName, Docs.Value vComment tipe fix) =
      Value vName vComment tipe fix

    toAlias (aName, Docs.Alias aComment args tipe) =
      Alias aName aComment args tipe

    toUnion (uName, Docs.Union uComment args cases) =
      Union uName uComment args cases
  in
    Documentation
      name
      comment
      (map toAlias (Map.toList aliases))
      (map toUnion (Map.toList unions))
      (map toValue (Map.toList values))
      (Just Version.version)



-- JSON


encode :: Documentation -> Encode.Value
encode (Documentation name comment aliases types values elmVersion) =
  Encode.object $
    [ "name" ==> Module.encode name
    , "comment" ==> Encode.text comment
    , "aliases" ==> Encode.list encodeAlias aliases
    , "types" ==> Encode.list encodeUnion types
    , "values" ==> Encode.list encodeValue values
    ]
    ++
      case elmVersion of
        Nothing ->
          []

        Just version ->
          [ "generated-with-elm-version" ==> Pkg.encodeVersion version
          ]


decoder :: Decode.Decoder Documentation
decoder =
  Documentation
    <$> Decode.field "name" Module.decoder
    <*> Decode.field "comment" Decode.text
    <*> Decode.field "aliases" (Decode.list aliasDecoder)
    <*> Decode.field "types" (Decode.list unionDecoder)
    <*> Decode.field "values" (Decode.list valueDecoder)
    <*> Decode.maybe (Decode.field "generated-with-elm-version" Pkg.versionDecoder)



-- JSON for ALIAS


encodeAlias :: Alias -> Encode.Value
encodeAlias (Alias name comment args tipe) =
  Encode.object
    [ "name" ==> Encode.text name
    , "comment" ==> Encode.text comment
    , "args" ==> Encode.list Encode.text args
    , "type" ==> Type.encode tipe
    ]


aliasDecoder :: Decode.Decoder Alias
aliasDecoder =
  Alias
    <$> Decode.field "name" Decode.text
    <*> Decode.field "comment" Decode.text
    <*> Decode.field "args" (Decode.list Decode.text)
    <*> Decode.field "type" Type.decoder



-- JSON for UNION


encodeUnion :: Union -> Encode.Value
encodeUnion (Union name comment args cases) =
  Encode.object
    [ "name" ==> Encode.text name
    , "comment" ==> Encode.text comment
    , "args" ==> Encode.list Encode.text args
    , "cases" ==> Encode.list encodeCase cases
    ]


unionDecoder :: Decode.Decoder Union
unionDecoder =
  Union
    <$> Decode.field "name" Decode.text
    <*> Decode.field "comment" Decode.text
    <*> Decode.field "args" (Decode.list Decode.text)
    <*> Decode.field "cases" (Decode.list caseDecoder)


encodeCase :: ( Text, [Type.Type] ) -> Encode.Value
encodeCase ( tag, args ) =
  Encode.list id [ Encode.text tag, Encode.list Type.encode args ]


caseDecoder :: Decode.Decoder ( Text, [Type.Type] )
caseDecoder =
  (,)
    <$> Decode.index 0 Decode.text
    <*> Decode.index 1 (Decode.list Type.decoder)



-- JSON for VALUE


encodeValue :: Value -> Encode.Value
encodeValue (Value name comment tipe assocPrec) =
  Encode.object $
    [ "name" ==> Encode.text name
    , "comment" ==> Encode.text comment
    , "type" ==> Type.encode tipe
    ]
    ++
      case assocPrec of
        Nothing ->
          []

        Just (assoc, prec) ->
          [ "associativity" ==> Encode.text assoc
          , "precedence" ==> Encode.int prec
          ]


valueDecoder :: Decode.Decoder Value
valueDecoder =
  Value
    <$> Decode.field "name" Decode.text
    <*> Decode.field "comment" Decode.text
    <*> Decode.field "type" Type.decoder
    <*> Decode.maybe (
      (,)
        <$> Decode.field "associativity" Decode.text
        <*> Decode.field "precedence" Decode.int
    )
