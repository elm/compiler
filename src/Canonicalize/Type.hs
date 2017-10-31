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
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning



-- RESULT


type Result i a =
  Result.Result i Warning.Warning Error.Error a



-- CANONICALIZE TYPES


canonicalize :: Env.Env -> Type.Raw -> Result () (A.Located Type.Canonical)
canonicalize env tipe@(A.A region _) =
  A.A region <$> canonicalizeHelp env tipe


canonicalizeHelp :: Env.Env -> Type.Raw -> Result () Type.Canonical
canonicalizeHelp env (A.A _ tipe) =
  case tipe of
    Type.RVar x ->
        Result.ok (Type.Var x)

    Type.RType region maybePrefix name args ->
        do  info <- Env.findType region env maybePrefix name (length args)
            cargs <- traverse (canonicalizeHelp env) args
            return $
              case info of
                Env.Alias home argNames aliasedType ->
                  Type.Aliased home name (zip argNames cargs) (Type.Holey aliasedType)

                Env.Union home ->
                  Type.Type home name cargs

    Type.RLambda a b ->
        Type.Lambda
          <$> canonicalizeHelp env a
          <*> canonicalizeHelp env b

    Type.RRecord fields ext ->
        do  fieldDict <- Dups.checkFields fields
            Type.Record
              <$> Map.traverseWithKey (\_ t -> canonicalizeHelp env t) fieldDict
              <*> traverse (canonicalizeHelp env) ext

    Type.RUnit ->
        Result.ok Type.Unit

    Type.RTuple a b cs ->
        Type.Tuple
          <$> canonicalizeHelp env a
          <*> canonicalizeHelp env b
          <*> traverse (canonicalizeHelp env) cs
