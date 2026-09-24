{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Type.Instantiate
  ( FreeVars
  , fromSrcType
  )
  where


import qualified Data.Map.Strict as Map
import qualified Data.Map.Utils as Map

import qualified AST.Canonical as Can
import qualified AST.Prim.TypeVar as T
import Type.Type



-- FREE VARS


type FreeVars =
  Map.Map T.Var Type



-- FROM SOURCE TYPE


fromSrcType :: Map.Map T.Var Type -> Can.Type -> IO Type
fromSrcType freeVars sourceType =
  case sourceType of
    Can.TLambda arg result ->
      FunN
        <$> fromSrcType freeVars arg
        <*> fromSrcType freeVars result

    Can.TVar name ->
      return $ $(Map.require 'fromSrcType) name freeVars T.varToChars

    Can.TType home name args ->
      AppN home name <$> traverse (fromSrcType freeVars) args

    Can.TAlias home name args aliasedType ->
      do  targs <- traverse (traverse (fromSrcType freeVars)) args
          AliasN home name targs <$>
            case aliasedType of
              Can.Filled realType ->
                fromSrcType freeVars realType

              Can.Holey realType ->
                fromSrcType (Map.fromList targs) realType

    Can.TTuple a b maybeC ->
      TupleN
        <$> fromSrcType freeVars a
        <*> fromSrcType freeVars b
        <*> traverse (fromSrcType freeVars) maybeC

    Can.TUnit ->
      return UnitN

    Can.TRecord fields maybeExt ->
      RecordN
        <$> traverse (fromSrcFieldType freeVars) fields
        <*>
          case maybeExt of
            Nothing  -> return EmptyRecordN
            Just ext -> return $ $(Map.require 'fromSrcType) ext freeVars T.varToChars


fromSrcFieldType :: Map.Map T.Var Type -> Can.FieldType -> IO Type
fromSrcFieldType freeVars (Can.FieldType _ tipe) =
  fromSrcType freeVars tipe
