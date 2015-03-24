{-# OPTIONS_GHC -Wall #-}
module Transform.Canonicalize.Type (tipe) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad.Error
import Data.Traversable (traverse)

import qualified AST.Type as T
import qualified AST.Variable as Var

import Transform.Canonicalize.Environment
import qualified Transform.Canonicalize.Variable as Canonicalize


tipe
    :: Environment
    -> T.RawType
    -> Canonicalizer String T.CanonicalType
tipe env typ =
    let go = tipe env
        goSnd (name,t) =
            (,) name <$> go t
    in
    case typ of
      T.Var x ->
          return (T.Var x)

      T.Type _ ->
          canonicalizeApp env typ []

      T.App t ts ->
          canonicalizeApp env t ts

      T.Lambda a b ->
          T.Lambda <$> go a <*> go b

      T.Record fields ext ->
          T.Record <$> mapM goSnd fields <*> traverse go ext

      T.Aliased _ _ _ ->
          error "a RawType should never have an alias in it"


canonicalizeApp
    :: Environment
    -> T.RawType
    -> [T.RawType]
    -> Canonicalizer String T.CanonicalType
canonicalizeApp env f args =
  case f of
    T.Type (Var.Raw rawName) ->
      do  answer <- Canonicalize.tvar env rawName
          case answer of
            Right alias ->
                canonicalizeAlias env alias args

            Left name ->
                case args of
                  [] -> return (T.Type name)
                  _ ->
                    T.App (T.Type name) <$> mapM (tipe env) args

    _ ->
      T.App <$> tipe env f <*> mapM (tipe env) args


canonicalizeAlias
    :: Environment
    -> (Var.Canonical, [String], T.CanonicalType)
    -> [T.RawType]
    -> Canonicalizer String T.CanonicalType
canonicalizeAlias env (name, tvars, dealiasedTipe) tipes =
  do  when (tipesLen /= tvarsLen) (throwError msg)
      tipes' <- mapM (tipe env) tipes
      return $ T.Aliased name (zip tvars tipes') (T.Holey dealiasedTipe)
  where
    tipesLen = length tipes
    tvarsLen = length tvars

    msg :: String
    msg =
        "Type alias '" ++ Var.toString name ++ "' expects " ++ show tvarsLen ++
        " type argument" ++ (if tvarsLen == 1 then "" else "s") ++
        " but was given " ++ show tipesLen
