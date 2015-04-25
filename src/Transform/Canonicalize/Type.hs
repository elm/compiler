{-# OPTIONS_GHC -Wall #-}
module Transform.Canonicalize.Type (tipe) where

import Control.Applicative ((<$>),(<*>))
import qualified Data.Traversable as Trav

import qualified AST.Type as T
import qualified AST.Variable as Var

import qualified Transform.Canonicalize.Environment as Env
import qualified Transform.Canonicalize.Error as Error
import qualified Transform.Canonicalize.Result as Result
import qualified Transform.Canonicalize.Variable as Canonicalize


tipe
    :: Env.Environment
    -> T.RawType
    -> Result.Result T.CanonicalType
tipe env typ =
    let go = tipe env
        goSnd (name,t) =
            (,) name <$> go t
    in
    case typ of
      T.Var x ->
          Result.ok (T.Var x)

      T.Type _ ->
          canonicalizeApp env typ []

      T.App t ts ->
          canonicalizeApp env t ts

      T.Lambda a b ->
          T.Lambda <$> go a <*> go b

      T.Record fields ext ->
          T.Record <$> Trav.traverse goSnd fields <*> Trav.traverse go ext

      T.Aliased _ _ _ ->
          error "a RawType should never have an alias in it"


canonicalizeApp
    :: Env.Environment
    -> T.RawType
    -> [T.RawType]
    -> Result.Result T.CanonicalType
canonicalizeApp env f args =
  case f of
    T.Type (Var.Raw rawName) ->
        Canonicalize.tvar env rawName
          `Result.andThen` canonicalizeWithTvar

    _ ->
        T.App <$> tipe env f <*> Trav.traverse (tipe env) args

  where
    canonicalizeWithTvar tvar =
        case tvar of
          Right alias ->
              canonicalizeAlias env alias args

          Left name ->
              case args of
                [] ->
                    Result.ok (T.Type name)
                _ ->
                    T.App (T.Type name) <$> Trav.traverse (tipe env) args


canonicalizeAlias
    :: Env.Environment
    -> (Var.Canonical, [String], T.CanonicalType)
    -> [T.RawType]
    -> Result.Result T.CanonicalType
canonicalizeAlias env (name, tvars, dealiasedTipe) types =
  if typesLen /= tvarsLen
    then Result.err (Error.alias name tvarsLen typesLen)
    else toAlias <$> Trav.traverse (tipe env) types
  where
    typesLen = length types
    tvarsLen = length tvars

    toAlias canonicalTypes =
        T.Aliased name (zip tvars canonicalTypes) (T.Holey dealiasedTipe)
