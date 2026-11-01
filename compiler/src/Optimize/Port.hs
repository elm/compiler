{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Optimize.Port
  ( toEncoder
  , toFlagsDecoder
  , toDecoder
  )
  where


import Prelude hiding (maybe, null)
import Control.Monad (foldM)
import qualified Data.NonEmptyMap as NEM

import qualified Array as A
import qualified Crash

import qualified AST.Optimized as Opt
import qualified AST.Prim.Fields as F
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified Data.Index as Index
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.String as ES
import qualified Optimize.Names as Names



-- ENCODE


toEncoder :: I.Type -> Names.Tracker Opt.Expr
toEncoder tipe =
  case tipe of
    I.TAlias _ _ _ xs a -> toEncoder (I.dealias xs a)
    I.TLambda _ _ _     -> $(Crash.crash 'toEncoder) "function"
    I.TVar _            -> $(Crash.crash 'toEncoder) "type variable"
    I.TUnit             -> Opt.func1 dollar <$> encode [N.ascii|null|]
    I.TPair   a b       -> encodePair a b
    I.TTriple a b c     -> encodeTriple a b c
    I.TType _ n _ xs    ->
      case A.size xs of
        0 | n == T.float  -> encode [N.ascii|float|]
          | n == T.int    -> encode [N.ascii|int|]
          | n == T.bool   -> encode [N.ascii|bool|]
          | n == T.string -> encode [N.ascii|string|]
          | n == T.value  -> Names.registerGlobal ModuleName.basics N.identity

        1 | n == T.maybe -> encodeMaybe (A.get 0 xs)
          | n == T.list  -> Opt.call1 <$> encode [N.ascii|list|]  <*> toEncoder (A.get 0 xs)
          | n == T.array -> Opt.call1 <$> encode [N.ascii|array|] <*> toEncoder (A.get 0 xs)

        _ ->
          $(Crash.crash 'toEncoder) "bad custom type"

    I.TEmpty ->
      do  object <- encode [N.ascii|object|]
          Names.registerKernel Module.kernel_list $
            Opt.func1 dollar (Opt.call1 object (Opt.List []))

    I.TRecord fields ->
      let
        encodeField (name, fieldType) =
          do  encoder <- toEncoder fieldType
              let value = Opt.call1 encoder (Opt.Access (Opt.VarLocal dollar) name)
              return $ Opt.Pair (Opt.Str (ES.fromName name)) value
      in
      do  object <- encode [N.ascii|object|]
          keyValuePairs <- traverse encodeField (NEM.toList (F.toNEM fields))
          Names.registerFields fields $
            Opt.func1 dollar (Opt.call1 object (Opt.List keyValuePairs))

    I.TExtRec _ _ ->
      $(Crash.crash 'toEncoder) "bad record"



-- ENCODE HELPERS


encodeMaybe :: I.Type -> Names.Tracker Opt.Expr
encodeMaybe tipe =
  do  null <- encode [N.ascii|null|]
      encoder <- toEncoder tipe
      destruct <- Names.registerGlobal ModuleName.maybe [N.ascii|destruct|]
      return $ Opt.func1 dollar $
        Opt.Call destruct null [ encoder, Opt.VarLocal dollar ]


encodePair :: I.Type -> I.Type -> Names.Tracker Opt.Expr
encodePair a b =
  let
    let_ arg index body =
      Opt.Destruct (Opt.Destructor arg (Opt.Index index (Opt.Root dollar))) body

    encodeArg arg tipe =
      do  encoder <- toEncoder tipe
          return $ Opt.call1 encoder (Opt.VarLocal arg)
  in
  do  list <- encode [N.ascii|list|]
      identity <- Names.registerGlobal ModuleName.basics N.identity
      arg1 <- encodeArg N.a a
      arg2 <- encodeArg N.b b
      return $ Opt.func1 dollar $
        let_ N.a Index.first $
        let_ N.b Index.second $
          Opt.Call list identity [ Opt.List [ arg1, arg2 ] ]


encodeTriple :: I.Type -> I.Type -> I.Type -> Names.Tracker Opt.Expr
encodeTriple a b c =
  let
    let_ arg index body =
      Opt.Destruct (Opt.Destructor arg (Opt.Index index (Opt.Root dollar))) body

    encodeArg arg tipe =
      do  encoder <- toEncoder tipe
          return $ Opt.call1 encoder (Opt.VarLocal arg)
  in
  do  list <- encode [N.ascii|list|]
      identity <- Names.registerGlobal ModuleName.basics N.identity
      arg1 <- encodeArg N.a a
      arg2 <- encodeArg N.b b
      arg3 <- encodeArg N.c c
      return $ Opt.func1 dollar $
        let_ N.a Index.first $
        let_ N.b Index.second $
        let_ N.c Index.third $
          Opt.Call list identity [ Opt.List [ arg1, arg2, arg3 ] ]


dollar :: N.Name
dollar = [N.ascii|$|]



-- FLAGS DECODER


toFlagsDecoder :: I.Type -> Names.Tracker Opt.Expr
toFlagsDecoder tipe =
  case tipe of
    I.TUnit ->
      do  succeed <- decode [N.ascii|succeed|]
          return $ Opt.call1 succeed Opt.Unit

    _ ->
      toDecoder tipe



-- DECODE


toDecoder :: I.Type -> Names.Tracker Opt.Expr
toDecoder tipe =
  case tipe of
    I.TLambda _ _ _     -> $(Crash.crash 'toDecoder) "functions should not be allowed through input ports"
    I.TVar _            -> $(Crash.crash 'toDecoder) "type variables should not be allowed through input ports"
    I.TAlias _ _ _ xs a -> toDecoder (I.dealias xs a)
    I.TUnit             -> decodeUnit
    I.TPair   a b       -> decodePair a b
    I.TTriple a b c     -> decodeTriple a b c
    I.TEmpty            -> decodeEmpty
    I.TRecord fs        -> decodeRecord fs
    I.TExtRec _ _       -> $(Crash.crash 'toDecoder) "bad record"
    I.TType _ n _ xs    ->
      case A.size xs of
        0 | n == T.float  -> decode [N.ascii|float|]
          | n == T.int    -> decode [N.ascii|int|]
          | n == T.bool   -> decode [N.ascii|bool|]
          | n == T.string -> decode [N.ascii|string|]
          | n == T.value  -> decode [N.ascii|value|]

        1 | n == T.maybe -> decodeMaybe (A.get 0 xs)
          | n == T.list  -> Opt.call1 <$> decode [N.ascii|list|]  <*> toDecoder (A.get 0 xs)
          | n == T.array -> Opt.call1 <$> decode [N.ascii|array|] <*> toDecoder (A.get 0 xs)

        _ ->
          $(Crash.crash 'toDecoder) "bad type"



-- DECODE MAYBE


decodeMaybe :: I.Type -> Names.Tracker Opt.Expr
decodeMaybe tipe =
  do  nothing <- Names.registerGlobal ModuleName.maybe [N.ascii|Nothing|]
      just    <- Names.registerGlobal ModuleName.maybe [N.ascii|Just|]

      oneOf <- decode [N.ascii|oneOf|]
      null  <- decode [N.ascii|null|]
      map_  <- decode [N.ascii|map|]

      subDecoder <- toDecoder tipe

      return $
        Opt.call1 oneOf $
          Opt.List
            [ Opt.call1 null nothing
            , Opt.call2 map_ just subDecoder
            ]



-- DECODE TUPLES


decodeUnit :: Names.Tracker Opt.Expr
decodeUnit =
  do  null <- decode [N.ascii|null|]
      return (Opt.call1 null Opt.Unit)


decodePair :: I.Type -> I.Type -> Names.Tracker Opt.Expr
decodePair a b =
  do  succeed <- decode [N.ascii|succeed|]
      indexAndThen 0 a =<<
        indexAndThen 1 b (Opt.call1 succeed (Opt.Pair (toLocal 0) (toLocal 1)))


decodeTriple :: I.Type -> I.Type -> I.Type -> Names.Tracker Opt.Expr
decodeTriple a b c =
  do  succeed <- decode [N.ascii|succeed|]
      indexAndThen 0 a =<<
        indexAndThen 1 b =<<
          indexAndThen 2 c (Opt.call1 succeed (Opt.Triple (toLocal 0) (toLocal 1) (toLocal 2)))


toLocal :: Int -> Opt.Expr
toLocal index =
  Opt.VarLocal (Names.fromVarIndex index)


indexAndThen :: Int -> I.Type -> Opt.Expr -> Names.Tracker Opt.Expr
indexAndThen i tipe decoder =
  do  andThen <- decode [N.ascii|andThen|]
      index <- decode [N.ascii|index|]
      typeDecoder <- toDecoder tipe
      return $
        Opt.Call andThen
          ( Opt.func1 (Names.fromVarIndex i) decoder
          )
          [ Opt.Call index (Opt.Int i) [typeDecoder]
          ]



-- DECODE RECORDS


decodeEmpty :: Names.Tracker Opt.Expr
decodeEmpty =
  do  succeed <- decode [N.ascii|succeed|]
      pure $ Opt.call1 succeed Opt.Empty


decodeRecord :: F.Fields I.Type -> Names.Tracker Opt.Expr
decodeRecord fields =
  let
    toFieldExpr name _ =
      Opt.VarLocal name

    record =
      Opt.Record (NEM.mapWithKey toFieldExpr (F.toNEM fields))
  in
  do  succeed <- decode [N.ascii|succeed|]
      foldM fieldAndThen (Opt.call1 succeed record) =<<
        Names.registerFields fields (NEM.toList (F.toNEM fields))


fieldAndThen :: Opt.Expr -> (N.Name, I.Type) -> Names.Tracker Opt.Expr
fieldAndThen decoder (key, tipe) =
  do  andThen <- decode [N.ascii|andThen|]
      field <- decode [N.ascii|field|]
      typeDecoder <- toDecoder tipe
      return $
        Opt.Call andThen
          (Opt.func1 key decoder)
          [ Opt.Call field (Opt.Str (ES.fromName key)) [typeDecoder]
          ]



-- GLOBALS HELPERS


encode :: N.Name -> Names.Tracker Opt.Expr
encode name =
  Names.registerGlobal ModuleName.jsonEncode name


decode :: N.Name -> Names.Tracker Opt.Expr
decode name =
  Names.registerGlobal ModuleName.jsonDecode name
