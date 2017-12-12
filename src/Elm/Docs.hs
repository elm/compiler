{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Docs
  ( Documentation
  , Module(..)
  , toDict
  , decoder
  , encode
  , Union(..)
  , Alias(..)
  , Value(..)
  , Binop(..)
  , Binop.Associativity(..)
  , Binop.Precedence(..)
  )
  where


import qualified Data.Map as Map
import Data.Text (Text)

import qualified AST.Utils.Binop as Binop
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Type as Type
import qualified Elm.Name as N
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode
import Json.Encode ((==>))



-- DOCUMENTATION


type Documentation =
  Map.Map N.Name Module


toDict :: [Module] -> Documentation
toDict modules =
  Map.fromList (map toPair modules)



toPair :: Module -> (N.Name, Module)
toPair modul@(Module name _ _ _ _ _) =
  (name, modul)



-- MODULES


data Module =
  Module
    { _name :: N.Name
    , _comment :: Text
    , _unions :: Map.Map N.Name Union
    , _aliases :: Map.Map N.Name Alias
    , _values :: Map.Map N.Name Value
    , _binops :: Map.Map N.Name Binop
    }


data Alias = Alias Text [N.Name] Type.Type
data Union = Union Text [N.Name] [(N.Name, [Type.Type])]
data Value = Value Text Type.Type
data Binop = Binop Text Type.Type Binop.Associativity Binop.Precedence



-- ENCODE


encode :: Module -> Encode.Value
encode (Module name comment unions aliases values binops) =
  Encode.object $
    [ "name" ==> Module.encode name
    , "comment" ==> Encode.text comment
    , "unions" ==> Encode.list encodeUnion (Map.toList unions)
    , "aliases" ==> Encode.list encodeAlias (Map.toList aliases)
    , "values" ==> Encode.list encodeValue (Map.toList values)
    , "binops" ==> Encode.list encodeBinop (Map.toList binops)
    ]



-- DECODE


decoder :: Decode.Decoder Module
decoder =
  Module
    <$> Decode.field "name" Module.decoder
    <*> Decode.field "comment" Decode.text
    <*> Decode.field "unions" (dictDecoder union)
    <*> Decode.field "aliases" (dictDecoder alias)
    <*> Decode.field "values" (dictDecoder value)
    <*> Decode.field "binops" (dictDecoder binop)


dictDecoder :: Decode.Decoder a -> Decode.Decoder (Map.Map N.Name a)
dictDecoder entryDecoder =
  Map.fromList <$> Decode.list (named entryDecoder)


named :: Decode.Decoder a -> Decode.Decoder (N.Name, a)
named entryDecoder =
  (,)
    <$> Decode.field "name" Decode.text
    <*> entryDecoder



-- UNION


encodeUnion :: (N.Name, Union) -> Encode.Value
encodeUnion (name, Union comment args cases) =
  Encode.object
    [ "name" ==> Encode.text name
    , "comment" ==> Encode.text comment
    , "args" ==> Encode.list Encode.text args
    , "cases" ==> Encode.list encodeCase cases
    ]


union :: Decode.Decoder Union
union =
  Union
    <$> Decode.field "comment" Decode.text
    <*> Decode.field "args" (Decode.list Decode.text)
    <*> Decode.field "cases" (Decode.list caseDecoder)



-- UNION CASE


encodeCase :: ( N.Name, [Type.Type] ) -> Encode.Value
encodeCase ( tag, args ) =
  Encode.list id [ Encode.text tag, Encode.list Type.encode args ]


caseDecoder :: Decode.Decoder ( N.Name, [Type.Type] )
caseDecoder =
  (,)
    <$> Decode.index 0 Decode.text
    <*> Decode.index 1 (Decode.list Type.decoder)



-- ALIAS


encodeAlias :: (N.Name, Alias) -> Encode.Value
encodeAlias ( name, Alias comment args tipe) =
  Encode.object
    [ "name" ==> Encode.text name
    , "comment" ==> Encode.text comment
    , "args" ==> Encode.list Encode.text args
    , "type" ==> Type.encode tipe
    ]


alias :: Decode.Decoder Alias
alias =
  Alias
    <$> Decode.field "comment" Decode.text
    <*> Decode.field "args" (Decode.list Decode.text)
    <*> Decode.field "type" Type.decoder



-- ENCODE VALUE


encodeValue :: (N.Name, Value) -> Encode.Value
encodeValue (name, Value comment tipe) =
  Encode.object
    [ "name" ==> Encode.text name
    , "comment" ==> Encode.text comment
    , "type" ==> Type.encode tipe
    ]


value :: Decode.Decoder Value
value =
  Value
    <$> Decode.field "comment" Decode.text
    <*> Decode.field "type" Type.decoder



-- BINOP


encodeBinop :: (N.Name, Binop) -> Encode.Value
encodeBinop (name, Binop comment tipe assoc prec) =
  Encode.object
    [ "name" ==> Encode.text name
    , "comment" ==> Encode.text comment
    , "type" ==> Type.encode tipe
    , "associativity" ==> encodeAssoc assoc
    , "precedence" ==> encodePrec prec
    ]


binop :: Decode.Decoder Binop
binop =
  Binop
    <$> Decode.field "comment" Decode.text
    <*> Decode.field "type" Type.decoder
    <*> Decode.field "associativity" assocDecoder
    <*> Decode.field "precedence" precDecoder



-- ASSOCIATIVITY


encodeAssoc :: Binop.Associativity -> Encode.Value
encodeAssoc assoc =
  case assoc of
    Binop.Left  -> Encode.text "left"
    Binop.Non   -> Encode.text "non"
    Binop.Right -> Encode.text "right"


assocDecoder :: Decode.Decoder Binop.Associativity
assocDecoder =
  do  txt <- Decode.text
      case txt of
        "left"  -> Decode.succeed Binop.Left
        "non"   -> Decode.succeed Binop.Non
        "right" -> Decode.succeed Binop.Right
        _       -> Decode.fail "Expecting an ASSOCIATIVITY (e.g. left, non, right)"



-- PRECEDENCE


encodePrec :: Binop.Precedence -> Encode.Value
encodePrec (Binop.Precedence n) =
  Encode.int n


precDecoder :: Decode.Decoder Binop.Precedence
precDecoder =
  Binop.Precedence <$> Decode.int
