{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Type (tipe) where

import Data.Text (Text)

import qualified AST.Type as T
import qualified AST.Variable as Var
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Variable as Canonicalize
import Canonicalize.Variable (Result)
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Error
import qualified Reporting.Region as R
import qualified Reporting.Result as Result



-- CANONICALIZE TYPES


tipe :: Env.Env -> T.Raw -> Result T.Canonical
tipe env (A.A _ typ) =
  let
    go =
      tipe env

    goField (A.A _ name, t) =
      (,) name <$> go t
  in
    case typ of
      T.RVar x ->
          Result.ok (T.Var x)

      T.RType name args ->
          canonicalizeApp env name args

      T.RLambda a b ->
          T.Lambda <$> go a <*> go b

      T.RRecord fields ext ->
          T.Record <$> traverse goField fields <*> traverse go ext


canonicalizeApp :: Env.Env -> A.Located Var.Raw -> [T.Raw] -> Result T.Canonical
canonicalizeApp env (A.A region (Var.Raw rawName)) args =
  do  tvar <- Canonicalize.tvar region env rawName
      case tvar of
        Right alias ->
          canonicalizeAlias region env alias args

        Left name ->
          T.Type name <$> traverse (tipe env) args


canonicalizeAlias
    :: R.Region
    -> Env.Env
    -> (Var.Canonical, [Text], T.Canonical)
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
      toAlias <$> traverse (tipe env) types

