{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Optimize.Port
  ( toEncoder
  , toFlagsDecoder
  , toDecoder
  )
  where


import Prelude hiding (maybe, null)
import Control.Monad (foldM)
import qualified Data.Map as Map

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Type as Type
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Optimize.DecisionTree as DT
import qualified Optimize.Names as Names



-- ENCODE


toEncoder :: Can.Type -> Names.Tracker Opt.Expr
toEncoder tipe =
  case tipe of
    Can.TAlias _ _ args alias ->
      toEncoder (Type.dealias args alias)

    Can.TLambda _ _ ->
      error "toEncoder: function"

    Can.TVar _ ->
      error "toEncoder: type variable"

    Can.TUnit ->
      encode "null"

    Can.TTuple a b c ->
      encodeTuple a b c

    Can.TType _ name args ->
      case args of
        []
          | name == N.float  -> encode "float"
          | name == N.int    -> encode "int"
          | name == N.bool   -> encode "bool"
          | name == N.string -> encode "string"
          | name == N.value  -> encode "value"

        [arg]
          | name == N.maybe -> encodeMaybe arg
          | name == N.list  -> encodeList arg
          | name == N.array -> encodeArray arg

        _ ->
          error "toEncoder: bad union type"

    Can.TRecord _ (Just _) ->
      error "toEncoder: bad record"

    Can.TRecord fields Nothing ->
      let
        encodeField (name, fieldType) =
          do  encoder <- toEncoder fieldType
              let value = Opt.Call encoder [Opt.Access (Opt.VarLocal N.dollar) name]
              return $ Opt.Tuple (Opt.Str name) value Nothing
      in
      do  object <- encode "object"
          keyValuePairs <- traverse encodeField (Map.toList fields)
          Names.registerFieldDict fields $
            Opt.Function [N.dollar] (Opt.Call object [Opt.List keyValuePairs])



-- ENCODE HELPERS


encodeMaybe :: Can.Type -> Names.Tracker Opt.Expr
encodeMaybe tipe =
  do  null <- encode "null"
      encoder <- toEncoder tipe

      let test = (DT.Empty, DT.IsCtor 2 "Just" Index.first)
      let just = Opt.Call encoder [ Opt.Index (Opt.VarLocal N.dollar) 0 ]
      let leaf expr = Opt.Leaf (Opt.Inline expr)

      return $ Opt.Function [N.dollar] $
        Opt.Case N.dollar N.dollar (Opt.Chain [test] (leaf just) (leaf null)) []


encodeList :: Can.Type -> Names.Tracker Opt.Expr
encodeList tipe =
  do  list <- encode "list"
      map_ <- Names.registerGlobal ModuleName.list "map"
      encoder <- toEncoder tipe
      return $
        Opt.Function [N.dollar] $
          Opt.Call list [ Opt.Call map_ [ encoder, Opt.VarLocal N.dollar ] ]


encodeArray :: Can.Type -> Names.Tracker Opt.Expr
encodeArray tipe =
  do  array <- encode "array"
      map_ <- Names.registerGlobal ModuleName.array "map"
      encoder <- toEncoder tipe
      return $
        Opt.Function [N.dollar] $
          Opt.Call array [ Opt.Call map_ [ encoder, Opt.VarLocal N.dollar ] ]


encodeTuple :: Can.Type -> Can.Type -> Maybe Can.Type -> Names.Tracker Opt.Expr
encodeTuple a b maybeC =
  let
    convert index tipe =
      do  encoder <- toEncoder tipe
          return $ Opt.Call encoder [ Opt.Index (Opt.VarLocal N.dollar) index ]
  in
  Opt.Function [N.dollar] <$> (
    Opt.Call
      <$> encode "list"
      <*>
        case maybeC of
          Nothing ->
            sequence [ convert 0 a, convert 1 b ]

          Just c ->
            sequence [ convert 0 a, convert 1 b, convert 2 c ]
  )



-- FLAGS DECODER


toFlagsDecoder :: Can.Type -> Names.Tracker Opt.Expr
toFlagsDecoder tipe =
  case tipe of
    Can.TUnit ->
      do  succeed <- decode "succeed"
          return $ Opt.Call succeed [ Opt.Unit ]

    _ ->
      toDecoder tipe



-- DECODE


toDecoder :: Can.Type -> Names.Tracker Opt.Expr
toDecoder tipe =
  case tipe of
    Can.TLambda _ _ ->
      error "functions should not be allowed through input ports"

    Can.TVar _ ->
      error "type variables should not be allowed through input ports"

    Can.TAlias _ _ args alias ->
      toDecoder (Type.dealias args alias)

    Can.TUnit ->
      decodeTuple0

    Can.TTuple a b c ->
      decodeTuple a b c

    Can.TType _ name args ->
      case args of
        []
          | name == N.float  -> decode "float"
          | name == N.int    -> decode "int"
          | name == N.bool   -> decode "bool"
          | name == N.string -> decode "string"
          | name == N.value  -> decode "value"

        [arg]
          | name == N.maybe -> decodeMaybe arg
          | name == N.list  -> decodeList arg
          | name == N.array -> decodeArray arg

        _ ->
          error "toDecoder: bad type"

    Can.TRecord _ (Just _) ->
      error "toDecoder: bad record"

    Can.TRecord fields Nothing ->
      decodeRecord fields



-- DECODE MAYBE


decodeMaybe :: Can.Type -> Names.Tracker Opt.Expr
decodeMaybe tipe =
  do  nothing <- Names.registerGlobal ModuleName.maybe "Nothing"
      just    <- Names.registerGlobal ModuleName.maybe "Just"

      oneOf <- decode "oneOf"
      null  <- decode "null"
      map_  <- decode "map"

      subDecoder <- toDecoder tipe

      return $
        Opt.Call oneOf
          [ Opt.List
              [ Opt.Call null [ nothing ]
              , Opt.Call map_ [ just, subDecoder ]
              ]
          ]


-- DECODE LIST


decodeList :: Can.Type -> Names.Tracker Opt.Expr
decodeList tipe =
  do  list <- decode "list"
      decoder <- toDecoder tipe
      return $ Opt.Call list [ decoder ]



-- DECODE ARRAY


decodeArray :: Can.Type -> Names.Tracker Opt.Expr
decodeArray tipe =
  do  array <- decode "array"
      decoder <- toDecoder tipe
      return $ Opt.Call array [ decoder ]



-- DECODE TUPLES


decodeTuple0 :: Names.Tracker Opt.Expr
decodeTuple0 =
  do  null <- decode "null"
      return (Opt.Call null [ Opt.Unit ])


decodeTuple :: Can.Type -> Can.Type -> Maybe Can.Type -> Names.Tracker Opt.Expr
decodeTuple a b maybeC =
  do  succeed <- decode "succeed"
      case maybeC of
        Nothing ->
          let tuple = Opt.Tuple (toLocal 0) (toLocal 1) Nothing in
          indexAndThen 0 a =<<
            indexAndThen 1 b (Opt.Call succeed [tuple])

        Just c ->
          let tuple = Opt.Tuple (toLocal 0) (toLocal 1) (Just (toLocal 2)) in
          indexAndThen 0 a =<<
            indexAndThen 1 b =<<
              indexAndThen 2 c (Opt.Call succeed [tuple])


toLocal :: Int -> Opt.Expr
toLocal index =
  Opt.VarLocal (N.addIndex "x" index)


indexAndThen :: Int -> Can.Type -> Opt.Expr -> Names.Tracker Opt.Expr
indexAndThen i tipe decoder =
  do  andThen <- decode "andThen"
      index <- decode "index"
      typeDecoder <- toDecoder tipe
      return $
        Opt.Call andThen
          [ Opt.Function [N.addIndex "x" i] decoder
          , Opt.Call index [ Opt.Int i, typeDecoder ]
          ]



-- DECODE RECORDS


decodeRecord :: Map.Map N.Name Can.Type -> Names.Tracker Opt.Expr
decodeRecord fields =
  let
    toFieldExpr name _ =
      Opt.VarLocal name

    record =
      Opt.Record (Map.mapWithKey toFieldExpr fields)
  in
    do  succeed <- decode "succeed"
        foldM fieldAndThen (Opt.Call succeed [record]) =<<
          Names.registerFieldDict fields (Map.toList fields)


fieldAndThen :: Opt.Expr -> (N.Name, Can.Type) -> Names.Tracker Opt.Expr
fieldAndThen decoder (key, tipe) =
  do  andThen <- decode "andThen"
      field <- decode "field"
      typeDecoder <- toDecoder tipe
      return $
        Opt.Call andThen
          [ Opt.Function [key] decoder
          , Opt.Call field [ Opt.Str key, typeDecoder ]
          ]



-- GLOBALS HELPERS


encode :: N.Name -> Names.Tracker Opt.Expr
encode name =
  Names.registerGlobal ModuleName.jsonEncode name


decode :: N.Name -> Names.Tracker Opt.Expr
decode name =
  Names.registerGlobal ModuleName.jsonDecode name
