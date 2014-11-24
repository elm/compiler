{-# OPTIONS_GHC -Wall #-}
module Transform.Canonicalize.Type (tipe) where

import Control.Arrow (second)
import Control.Applicative ((<$>),(<*>))
import Control.Monad.Error
import qualified Data.Map as Map
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
    let go = tipe env in
    case typ of
      T.Var x ->
          return (T.Var x)

      T.Type _ ->
          canonicalizeApp env typ []

      T.App t ts ->
          canonicalizeApp env t ts

      T.Lambda a b ->
          T.Lambda <$> go a <*> go b

      T.Aliased name t ->
          T.Aliased name <$> go t

      T.Record fields ext ->
          let go' (f,t) = (,) f <$> go t
          in
              T.Record <$> mapM go' fields <*> traverse go ext


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
      let tipe' = replace (Map.fromList (zip tvars tipes')) dealiasedTipe
      return $ T.Aliased name tipe'
  where
    tipesLen = length tipes
    tvarsLen = length tvars

    msg :: String
    msg = "Type alias '" ++ Var.toString name ++ "' expects " ++ show tvarsLen ++
          " type argument" ++ (if tvarsLen == 1 then "" else "s") ++
          " but was given " ++ show tipesLen

    replace :: Map.Map String T.CanonicalType -> T.CanonicalType -> T.CanonicalType
    replace typeTable t =
        let go = replace typeTable in
        case t of
          T.Lambda a b          -> T.Lambda (go a) (go b)
          T.Var x               -> Map.findWithDefault t x typeTable
          T.Record fields ext   -> T.Record (map (second go) fields) (fmap go ext)
          T.Aliased original t' -> T.Aliased original (go t')
          T.Type _              -> t
          T.App f args          -> T.App (go f) (map go args)
