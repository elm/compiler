{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Docs
  ( Documentation
  , Module(..)
  , toDict
  , decoder
  , encode
  , Docs.Docs(..)
  , Docs.Checked
  , Docs.Entry(..)
  , Docs.Union(..)
  , Docs.Alias(..)
  , Docs.Value(..)
  , Decl.Assoc(..)
  , Decl.assocToText
  )
  where


import qualified Data.Map as Map
import Data.Text (Text)

import qualified AST.Declaration as Decl
import qualified Docs.AST as Docs
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Type as Type
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode
import Json.Encode ((==>))



-- DOCUMENTATION


type Documentation =
  Map.Map Module.Raw Docs.Checked


data Module =
  Module
    { _name :: !Module.Raw
    , _docs :: !Docs.Checked
    }


toDict :: [Module] -> Documentation
toDict modules =
  Map.fromList $ map (\(Module name docs) -> (name, docs)) modules



-- ENCODE


encode :: Module -> Encode.Value
encode (Module name (Docs.Docs overview unions aliases values)) =
  Encode.object $
    [ "name" ==> Module.encode name
    , "comment" ==> Encode.text overview
    , "types" ==> Encode.list encodeUnion (Map.toList unions)
    , "aliases" ==> Encode.list encodeAlias (Map.toList aliases)
    , "values" ==> Encode.list encodeValue (Map.toList values)
    ]



-- DECODE


decoder :: Decode.Decoder Module
decoder =
  Module
    <$> Decode.field "name" Module.decoder
    <*> decodeDocs


decodeDocs :: Decode.Decoder Docs.Checked
decodeDocs =
  Docs.Docs
    <$> Decode.field "comment" Decode.text
    <*> Decode.field "types" (entryDictDecoder unionDecoder)
    <*> Decode.field "aliases" (entryDictDecoder aliasDecoder)
    <*> Decode.field "values" (entryDictDecoder valueDecoder)


entryDictDecoder :: Decode.Decoder a -> Decode.Decoder (Map.Map Text (Docs.Good a))
entryDictDecoder subDecoder =
  Map.fromList <$> Decode.list (entryDecoder subDecoder)


entryDecoder :: Decode.Decoder a -> Decode.Decoder ( Text, Docs.Good a )
entryDecoder subDecoder =
  Decode.map2 (,) (Decode.field "name" Decode.text) $
    Decode.map2 Docs.Entry (Decode.field "comment" Decode.text) $
      subDecoder



-- ENCODE UNION


encodeUnion :: ( Text, Docs.Good Docs.Union ) -> Encode.Value
encodeUnion ( name, Docs.Entry comment (Docs.Union args cases) ) =
  Encode.object
    [ "name" ==> Encode.text name
    , "comment" ==> Encode.text comment
    , "args" ==> Encode.list Encode.text args
    , "cases" ==> Encode.list encodeCase cases
    ]


encodeCase :: ( Text, [Type.Type] ) -> Encode.Value
encodeCase ( tag, args ) =
  Encode.list id [ Encode.text tag, Encode.list Type.encode args ]



-- DECODE UNION


unionDecoder :: Decode.Decoder Docs.Union
unionDecoder =
  Docs.Union
    <$> Decode.field "args" (Decode.list Decode.text)
    <*> Decode.field "cases" (Decode.list caseDecoder)


caseDecoder :: Decode.Decoder ( Text, [Type.Type] )
caseDecoder =
  (,)
    <$> Decode.index 0 Decode.text
    <*> Decode.index 1 (Decode.list Type.decoder)



-- ENCODE ALIAS


encodeAlias :: ( Text, Docs.Good Docs.Alias ) -> Encode.Value
encodeAlias ( name, Docs.Entry comment (Docs.Alias args tipe) ) =
  Encode.object
    [ "name" ==> Encode.text name
    , "comment" ==> Encode.text comment
    , "args" ==> Encode.list Encode.text args
    , "type" ==> Type.encode tipe
    ]



-- DECODE ALIAS


aliasDecoder :: Decode.Decoder Docs.Alias
aliasDecoder =
  Docs.Alias
    <$> Decode.field "args" (Decode.list Decode.text)
    <*> Decode.field "type" Type.decoder



-- ENCODE VALUE


encodeValue :: ( Text, Docs.Good Docs.GoodValue ) -> Encode.Value
encodeValue ( name, Docs.Entry comment value ) =
  Encode.object $
    [ "name" ==> Encode.text name
    , "comment" ==> Encode.text comment
    ]
    ++
      case value of
        Docs.Value tipe ->
          [ "type" ==> Type.encode tipe
          ]

        Docs.Infix tipe assoc prec ->
          [ "type" ==> Type.encode tipe
          , "associativity" ==> Encode.text (Decl.assocToText assoc)
          , "precedence" ==> Encode.int prec
          ]



-- DECODE VALUE


valueDecoder :: Decode.Decoder Docs.GoodValue
valueDecoder =
  do  tipe <- Decode.field "type" Type.decoder
      Decode.oneOf
        [ Docs.Infix tipe
            <$> Decode.field "associativity" assocDecoder
            <*> Decode.field "precedence" Decode.int
        , Decode.succeed (Docs.Value tipe)
        ]


assocDecoder :: Decode.Decoder Decl.Assoc
assocDecoder =
  do  txt <- Decode.text
      case txt of
        "left"  -> Decode.succeed Decl.Left
        "non"   -> Decode.succeed Decl.Non
        "right" -> Decode.succeed Decl.Right
        _       -> Decode.fail "Expecting an ASSOCIATIVITY (e.g. left, non, right)"
