{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Type (tipe) where

import qualified Data.Traversable as Trav

import qualified AST.Type as T
import qualified AST.Variable as Var

import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Variable as Canonicalize
import Canonicalize.Variable (Result)


tipe :: Env.Environment -> T.Raw -> Result T.Canonical
tipe env annType@(A.A _ typ) =
  let
    go =
      tipe env

    goSnd (name,t) =
      (,) name <$> go t
  in
    case typ of
      T.RVar x ->
          Result.ok (T.Var x)

      T.RType _ ->
          canonicalizeApp env annType []

      T.RApp t ts ->
          canonicalizeApp env t ts

      T.RLambda a b ->
          T.Lambda <$> go a <*> go b

      T.RRecord fields ext ->
          T.Record <$> Trav.traverse goSnd fields <*> Trav.traverse go ext


canonicalizeApp :: Env.Environment -> T.Raw -> [T.Raw] -> Result T.Canonical
canonicalizeApp env annFunc@(A.A region func) args =
  case func of
    T.RType (Var.Raw rawName) ->
      canonicalizeWithTvar =<< Canonicalize.tvar region env rawName

    _ ->
      T.App
        <$> tipe env annFunc
        <*> Trav.traverse (tipe env) args

  where
    canonicalizeWithTvar tvar =
        case tvar of
          Right alias ->
              canonicalizeAlias region env alias args

          Left name ->
              case args of
                [] ->
                    Result.ok (T.Type name)

                _ ->
                    T.App (T.Type name) <$> Trav.traverse (tipe env) args


canonicalizeAlias
    :: R.Region
    -> Env.Environment
    -> (Var.Canonical, [String], T.Canonical)
    -> [T.Raw]
    -> Result T.Canonical
canonicalizeAlias region env (name, tvars, dealiasedTipe) types =
  let
    typesLen =
      length types

    tvarsLen =
      length tvars

    toAlias canonicalTypes =
      T.Aliased name (zip tvars canonicalTypes) (T.Holey dealiasedTipe)
  in
    if typesLen /= tvarsLen then
      Result.throw region (Error.alias name tvarsLen typesLen)

    else
      toAlias <$> Trav.traverse (tipe env) types

