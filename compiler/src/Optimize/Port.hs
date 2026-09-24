{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Optimize.Port
  ( toEncoder
  , toFlagsDecoder
  , toDecoder
  )
  where


import Prelude hiding (maybe, null)
import Control.Monad (foldM)
import qualified Data.Map as Map

import qualified Crash

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeName as T
import qualified AST.Utils.Type as Type
import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Elm.String as ES
import qualified Optimize.Names as Names



-- ENCODE


toEncoder :: Can.Type -> Names.Tracker Opt.Expr
toEncoder tipe =
  case tipe of
    Can.TAlias _ _ args alias ->
      toEncoder (Type.dealias args alias)

    Can.TLambda _ _ ->
      $(Crash.crash 'toEncoder) "function"

    Can.TVar _ ->
      $(Crash.crash 'toEncoder) "type variable"

    Can.TUnit ->
      Opt.Function [dollar] <$> encode [N.ascii|null|]

    Can.TTuple a b c ->
      encodeTuple a b c

    Can.TType _ name args ->
      case args of
        []
          | name == T.float  -> encode [N.ascii|float|]
          | name == T.int    -> encode [N.ascii|int|]
          | name == T.bool   -> encode [N.ascii|bool|]
          | name == T.string -> encode [N.ascii|string|]
          | name == T.value  -> Names.registerGlobal ModuleName.basics N.identity

        [arg]
          | name == T.maybe -> encodeMaybe arg
          | name == T.list  -> encodeList arg
          | name == T.array -> encodeArray arg

        _ ->
          $(Crash.crash 'toEncoder) "bad custom type"

    Can.TRecord _ (Just _) ->
      $(Crash.crash 'toEncoder) "bad record"

    Can.TRecord fields Nothing ->
      let
        encodeField (name, Can.FieldType _ fieldType) =
          do  encoder <- toEncoder fieldType
              let value = Opt.Call encoder [Opt.Access (Opt.VarLocal dollar) name]
              return $ Opt.Tuple (Opt.Str (ES.fromName name)) value Nothing
      in
      do  object <- encode [N.ascii|object|]
          keyValuePairs <- traverse encodeField (Map.toList fields)
          Names.registerFieldDict fields $
            Opt.Function [dollar] (Opt.Call object [Opt.List keyValuePairs])



-- ENCODE HELPERS


encodeMaybe :: Can.Type -> Names.Tracker Opt.Expr
encodeMaybe tipe =
  do  null <- encode [N.ascii|null|]
      encoder <- toEncoder tipe
      destruct <- Names.registerGlobal ModuleName.maybe [N.ascii|destruct|]
      return $ Opt.Function [dollar] $
        Opt.Call destruct [ null, encoder, Opt.VarLocal dollar ]


encodeList :: Can.Type -> Names.Tracker Opt.Expr
encodeList tipe =
  do  list <- encode [N.ascii|list|]
      encoder <- toEncoder tipe
      return $ Opt.Call list [ encoder ]


encodeArray :: Can.Type -> Names.Tracker Opt.Expr
encodeArray tipe =
  do  array <- encode [N.ascii|array|]
      encoder <- toEncoder tipe
      return $ Opt.Call array [ encoder ]


encodeTuple :: Can.Type -> Can.Type -> Maybe Can.Type -> Names.Tracker Opt.Expr
encodeTuple a b maybeC =
  let
    let_ arg index body =
      Opt.Destruct (Opt.Destructor arg (Opt.Index index (Opt.Root dollar))) body

    encodeArg arg tipe =
      do  encoder <- toEncoder tipe
          return $ Opt.Call encoder [ Opt.VarLocal arg ]
  in
  do  list <- encode [N.ascii|list|]
      identity <- Names.registerGlobal ModuleName.basics N.identity
      arg1 <- encodeArg [N.ascii|a|] a
      arg2 <- encodeArg [N.ascii|b|] b

      case maybeC of
        Nothing ->
          return $ Opt.Function [dollar] $
            let_ [N.ascii|a|] Index.first $
            let_ [N.ascii|b|] Index.second $
              Opt.Call list [ identity, Opt.List [ arg1, arg2 ] ]

        Just c ->
          do  arg3 <- encodeArg [N.ascii|c|] c
              return $ Opt.Function [dollar] $
                let_ [N.ascii|a|] Index.first $
                let_ [N.ascii|b|] Index.second $
                let_ [N.ascii|c|] Index.third $
                  Opt.Call list [ identity, Opt.List [ arg1, arg2, arg3 ] ]


dollar :: N.Name
dollar = [N.ascii|$|]



-- FLAGS DECODER


toFlagsDecoder :: Can.Type -> Names.Tracker Opt.Expr
toFlagsDecoder tipe =
  case tipe of
    Can.TUnit ->
      do  succeed <- decode [N.ascii|succeed|]
          return $ Opt.Call succeed [ Opt.Unit ]

    _ ->
      toDecoder tipe



-- DECODE


toDecoder :: Can.Type -> Names.Tracker Opt.Expr
toDecoder tipe =
  case tipe of
    Can.TLambda _ _ ->
      $(Crash.crash 'toDecoder) "functions should not be allowed through input ports"

    Can.TVar _ ->
      $(Crash.crash 'toDecoder) "type variables should not be allowed through input ports"

    Can.TAlias _ _ args alias ->
      toDecoder (Type.dealias args alias)

    Can.TUnit ->
      decodeTuple0

    Can.TTuple a b c ->
      decodeTuple a b c

    Can.TType _ name args ->
      case args of
        []
          | name == T.float  -> decode [N.ascii|float|]
          | name == T.int    -> decode [N.ascii|int|]
          | name == T.bool   -> decode [N.ascii|bool|]
          | name == T.string -> decode [N.ascii|string|]
          | name == T.value  -> decode [N.ascii|value|]

        [arg]
          | name == T.maybe -> decodeMaybe arg
          | name == T.list  -> decodeList arg
          | name == T.array -> decodeArray arg

        _ ->
          $(Crash.crash 'toDecoder) "bad type"

    Can.TRecord _ (Just _) ->
      $(Crash.crash 'toDecoder) "bad record"

    Can.TRecord fields Nothing ->
      decodeRecord fields



-- DECODE MAYBE


decodeMaybe :: Can.Type -> Names.Tracker Opt.Expr
decodeMaybe tipe =
  do  nothing <- Names.registerGlobal ModuleName.maybe [N.ascii|Nothing|]
      just    <- Names.registerGlobal ModuleName.maybe [N.ascii|Just|]

      oneOf <- decode [N.ascii|oneOf|]
      null  <- decode [N.ascii|null|]
      map_  <- decode [N.ascii|map|]

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
  do  list <- decode [N.ascii|list|]
      decoder <- toDecoder tipe
      return $ Opt.Call list [ decoder ]



-- DECODE ARRAY


decodeArray :: Can.Type -> Names.Tracker Opt.Expr
decodeArray tipe =
  do  array <- decode [N.ascii|array|]
      decoder <- toDecoder tipe
      return $ Opt.Call array [ decoder ]



-- DECODE TUPLES


decodeTuple0 :: Names.Tracker Opt.Expr
decodeTuple0 =
  do  null <- decode [N.ascii|null|]
      return (Opt.Call null [ Opt.Unit ])


decodeTuple :: Can.Type -> Can.Type -> Maybe Can.Type -> Names.Tracker Opt.Expr
decodeTuple a b maybeC =
  do  succeed <- decode [N.ascii|succeed|]
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
  Opt.VarLocal (Names.fromVarIndex index)


indexAndThen :: Int -> Can.Type -> Opt.Expr -> Names.Tracker Opt.Expr
indexAndThen i tipe decoder =
  do  andThen <- decode [N.ascii|andThen|]
      index <- decode [N.ascii|index|]
      typeDecoder <- toDecoder tipe
      return $
        Opt.Call andThen
          [ Opt.Function [Names.fromVarIndex i] decoder
          , Opt.Call index [ Opt.Int i, typeDecoder ]
          ]



-- DECODE RECORDS


decodeRecord :: Map.Map N.Name Can.FieldType -> Names.Tracker Opt.Expr
decodeRecord fields =
  let
    toFieldExpr name _ =
      Opt.VarLocal name

    record =
      Opt.Record (Map.mapWithKey toFieldExpr fields)
  in
    do  succeed <- decode [N.ascii|succeed|]
        foldM fieldAndThen (Opt.Call succeed [record]) =<<
          Names.registerFieldDict fields (Map.toList fields)


fieldAndThen :: Opt.Expr -> (N.Name, Can.FieldType) -> Names.Tracker Opt.Expr
fieldAndThen decoder (key, Can.FieldType _ tipe) =
  do  andThen <- decode [N.ascii|andThen|]
      field <- decode [N.ascii|field|]
      typeDecoder <- toDecoder tipe
      return $
        Opt.Call andThen
          [ Opt.Function [key] decoder
          , Opt.Call field [ Opt.Str (ES.fromName key), typeDecoder ]
          ]



-- GLOBALS HELPERS


encode :: N.Name -> Names.Tracker Opt.Expr
encode name =
  Names.registerGlobal ModuleName.jsonEncode name


decode :: N.Name -> Names.Tracker Opt.Expr
decode name =
  Names.registerGlobal ModuleName.jsonDecode name
