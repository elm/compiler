{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Canonicalize.Type
  ( canonicalize
  )
  where


import qualified Data.Map as Map

import qualified AST.Type as Type
import qualified Canonicalize.Environment.Dups as Dups
import qualified Canonicalize.Environment.Internals as Env
import qualified Reporting.Annotation as A
import qualified Reporting.Result as Result



-- CANONICALIZE TYPES


canonicalize :: Env.Env -> Type.Raw -> Env.Result Type.Canonical
canonicalize env (A.A _ tipe) =
  case tipe of
    Type.RVar x ->
        Result.ok (Type.Var x)

    Type.RType region maybePrefix name args ->
        do  info <- Env.findType region env maybePrefix name (length args)
            cargs <- traverse (canonicalize env) args
            return $
              case info of
                Env.Alias home argNames aliasedType ->
                  Type.Aliased home name (zip argNames cargs) (Type.Holey aliasedType)

                Env.Union home ->
                  Type.Type home name cargs

    Type.RLambda a b ->
        Type.Lambda
          <$> canonicalize env a
          <*> canonicalize env b

    Type.RRecord fields ext ->
        do  fieldDict <- Dups.checkFields fields
            Type.Record
              <$> Map.traverseWithKey (\_ t -> canonicalize env t) fieldDict
              <*> traverse (canonicalize env) ext

    Type.RUnit ->
        Result.ok Type.Unit

    Type.RTuple a b cs ->
        Type.Tuple
          <$> canonicalize env a
          <*> canonicalize env b
          <*> traverse (canonicalize env) cs
