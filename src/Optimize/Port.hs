{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Optimize.Port
  ( toEncoder
  , toFlagsDecoder
  , toDecoder
  )
  where


import Prelude hiding (maybe, null)
import Control.Monad (foldM, zipWithM)
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Expression.Optimized as Opt
import qualified AST.Helpers as Help
import qualified AST.Literal as Literal
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as T
import qualified AST.Variable as Var
import qualified Optimize.DecisionTree as DT
import qualified Optimize.Environment as Env



-- ENCODE


toEncoder :: T.Canonical -> Env.Optimizer Opt.Expr
toEncoder tipe =
  case tipe of
    T.Aliased _ args t ->
      toEncoder (T.dealias args t)

    T.Lambda _ _ ->
      error "toEncoder: function"

    T.Var _ ->
      error "toEncoder: type variable"

    T.Type name []
      | Var.isPrim "Float"  name -> encode "float"
      | Var.isPrim "Int"    name -> encode "int"
      | Var.isPrim "Bool"   name -> encode "bool"
      | Var.isPrim "String" name -> encode "string"
      | Var.isJson name          -> encode "value"
      | Var.isTuple name         -> encode "null"
      | otherwise                -> error "toEncoder: bad type"

    T.Type name [arg]
      | Var.isMaybe name -> encodeMaybe arg
      | Var.isList  name -> encodeList arg
      | Var.isArray name -> encodeArray arg
      | otherwise        -> error "toEncoder: bad union type"

    T.Type name args
      | Var.isTuple name -> encodeTuple args

    T.Type _ _ ->
      error "toEncoder: bad union type"

    T.Record _ (Just _) ->
      error "toEncoder: bad record"

    T.Record fields Nothing ->
      let
        convert (field, typ) =
          do  encoder <- toEncoder typ
              return
                ( field
                , Opt.Call encoder [ Opt.Access (Opt.VarLocal "r") field ]
                )
      in
        do  record <- traverse convert fields
            return $ Opt.Function ["r"] (Opt.Record record)



-- ENCODE HELPERS


encodeMaybe :: T.Canonical -> Env.Optimizer Opt.Expr
encodeMaybe tipe =
  do  null <- encode "null"
      encoder <- toEncoder tipe

      let nothing = Var.inCore "Maybe" "Nothing"
      let test = (DT.Empty, DT.Constructor nothing)
      let just = Opt.Call encoder [ Opt.CtorAccess (Opt.VarLocal "m") 0 ]
      let leaf expr = Opt.Leaf (Opt.Inline expr)

      return $ Opt.Function ["m"] $
        Opt.Case "m" (Opt.Chain [test] (leaf null) (leaf just)) []


encodeList :: T.Canonical -> Env.Optimizer Opt.Expr
encodeList tipe =
  do  list <- encode "list"
      map_ <- core "List" "map"
      encoder <- toEncoder tipe
      return $
        Opt.Function ["xs"] $
          Opt.Call list [ Opt.Call map_ [ encoder, Opt.VarLocal "xs" ] ]


encodeArray :: T.Canonical -> Env.Optimizer Opt.Expr
encodeArray tipe =
  do  array <- encode "array"
      map_ <- core "Array" "map"
      encoder <- toEncoder tipe
      return $
        Opt.Function ["xs"] $
          Opt.Call array [ Opt.Call map_ [ encoder, Opt.VarLocal "xs" ] ]


encodeTuple :: [T.Canonical] -> Env.Optimizer Opt.Expr
encodeTuple tipes =
  let
    convert tipe index =
      do  encoder <- toEncoder tipe
          return $
            Opt.Call encoder [ Opt.CtorAccess (Opt.VarLocal "t") index ]
  in
    do  list <- encode "list"
        entries <- zipWithM convert tipes [ 0 .. length tipes ]
        return $ Opt.Function ["t"] $ Opt.Call list entries



-- FLAGS DECODER


toFlagsDecoder :: T.Canonical -> Env.Optimizer Opt.Expr
toFlagsDecoder tipe =
  case tipe of
    T.Type name [] | Var.isTuple name ->
      do  succeed <- decode "succeed"
          return $ Opt.Call succeed [ Opt.Ctor Help.zeroTuple [] ]

    _ ->
      toDecoder tipe



-- DECODE


toDecoder :: T.Canonical -> Env.Optimizer Opt.Expr
toDecoder tipe =
  case tipe of
    T.Lambda _ _ ->
      error "functions should not be allowed through input ports"

    T.Var _ ->
      error "type variables should not be allowed through input ports"

    T.Aliased _ args t ->
      toDecoder (T.dealias args t)

    T.Type name []
      | Var.isPrim "Float"  name -> decode "float"
      | Var.isPrim "Int"    name -> decode "int"
      | Var.isPrim "Bool"   name -> decode "bool"
      | Var.isPrim "String" name -> decode "string"
      | Var.isJson name          -> decode "value"
      | Var.isTuple name         -> decodeTuple0
      | otherwise                -> error "toDecoder: bad type"

    T.Type name [arg]
      | Var.isMaybe name -> decodeMaybe arg
      | Var.isList  name -> decodeList arg
      | Var.isArray name -> decodeArray arg
      | otherwise        -> error "toDecoder: bad union type"

    T.Type name args
      | Var.isTuple name -> decodeTuple args

    T.Type _ _ ->
      error "toDecoder: bad union type"

    T.Record _ (Just _) ->
      error "toDecoder: bad record"

    T.Record fields Nothing ->
      decodeRecord fields



-- DECODE MAYBE


decodeMaybe :: T.Canonical -> Env.Optimizer Opt.Expr
decodeMaybe tipe =
  do  let nothing = Var.Global (ModuleName.inCore "Maybe") "Nothing"
      let just    = Var.Global (ModuleName.inCore "Maybe") "Just"

      Env.register nothing
      Env.register just

      oneOf <- decode "oneOf"
      null  <- decode "null"
      map_  <- decode "map"

      subDecoder <- toDecoder tipe

      return $
        Opt.Call oneOf
          [ Opt.List
              [ Opt.Call null [ Opt.VarGlobal nothing ]
              , Opt.Call map_ [ Opt.VarGlobal just, subDecoder ]
              ]
          ]


-- DECODE LIST


decodeList :: T.Canonical -> Env.Optimizer Opt.Expr
decodeList tipe =
  do  list <- decode "list"
      decoder <- toDecoder tipe
      return $ Opt.Call list [ decoder ]



-- DECODE ARRAY


decodeArray :: T.Canonical -> Env.Optimizer Opt.Expr
decodeArray tipe =
  do  array <- decode "array"
      decoder <- toDecoder tipe
      return $ Opt.Call array [ decoder ]



-- DECODE TUPLES


decodeTuple0 :: Env.Optimizer Opt.Expr
decodeTuple0 =
  do  null <- decode "null"
      return (Opt.Call null [ Opt.Ctor Help.zeroTuple [] ])


decodeTuple :: [T.Canonical] -> Env.Optimizer Opt.Expr
decodeTuple types =
  let
    size =
      length types

    entries =
      zip [ 0 .. size - 1] types

    tuple =
      Opt.Ctor
        (Help.makeTuple size)
        (map (Opt.VarLocal . indexToName . fst) entries)
  in
    do  succeed <- decode "succeed"
        foldM indexAndThen (Opt.Call succeed [tuple]) entries


indexToName :: Int -> Text
indexToName index =
  Text.pack ('x' : show index)


indexAndThen :: Opt.Expr -> (Int, T.Canonical) -> Env.Optimizer Opt.Expr
indexAndThen decoder (i, tipe) =
  do  andThen <- decode "andThen"
      index <- decode "index"
      typeDecoder <- toDecoder tipe
      return $
        Opt.Call andThen
          [ Opt.Function [indexToName i] decoder
          , Opt.Call index [ Opt.Literal (Literal.IntNum i), typeDecoder ]
          ]



-- DECODE RECORDS


decodeRecord :: [(Text, T.Canonical)] -> Env.Optimizer Opt.Expr
decodeRecord fields =
  let
    toFieldExpr (key, _) =
      (key, Opt.VarLocal key)

    record =
      Opt.Record (map toFieldExpr fields)
  in
    do  succeed <- decode "succeed"
        foldM fieldAndThen (Opt.Call succeed [record]) fields


fieldAndThen :: Opt.Expr -> (Text, T.Canonical) -> Env.Optimizer Opt.Expr
fieldAndThen decoder (key, tipe) =
  do  andThen <- decode "andThen"
      field <- decode "field"
      typeDecoder <- toDecoder tipe
      return $
        Opt.Call andThen
          [ Opt.Function [key] decoder
          , Opt.Call field [ Opt.Literal (Literal.Str key), typeDecoder ]
          ]



-- GLOBALS HELPERS


core :: Text -> Text -> Env.Optimizer Opt.Expr
core home name =
  do  let var = Var.Global (ModuleName.inCore home) name
      Env.register var
      return (Opt.VarGlobal var)


encode :: Text -> Env.Optimizer Opt.Expr
encode name =
  core "Json.Encode" name


decode :: Text -> Env.Optimizer Opt.Expr
decode name =
  core "Json.Decode" name
