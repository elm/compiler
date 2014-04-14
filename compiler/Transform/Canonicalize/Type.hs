{-# OPTIONS_GHC -Wall #-}
module Transform.Canonicalize.Type where

import Control.Arrow (second)
import Control.Applicative ((<$>),(<*>))
import qualified Data.Map as Map
import Data.Traversable (traverse)

import qualified AST.Type as T
import qualified AST.Variable as Var

import Transform.Canonicalize.Environment
import Transform.Canonicalize.Variable (tvar)

tipe :: Environment -> T.RawType -> Either String T.CanonicalType
tipe env typ =
    let go = tipe env in
    case typ of
      T.Lambda a b ->
          T.Lambda <$> go a <*> go b

      T.Var x ->
          return (T.Var x)

      T.Record fields ext ->
          T.Record <$> mapM go' fields <*> traverse go ext
        where
          go' (f,t) = (,) f <$> go t

      T.Aliased name t ->
          T.Aliased name <$> go t

      T.Data (Var.Raw name) ts ->
          case tvar env name of
            Left err -> Left err

            Right (Left name') ->
                T.Data name' <$> mapM go ts

            Right (Right (name', tvars, t))
                | tsLen /= tvarsLen -> Left msg
                | otherwise ->
                    do ts' <- mapM go ts
                       let t' = replace (Map.fromList (zip tvars ts')) t
                       return $ T.Aliased name' t'
                where
                  tsLen    = length ts
                  tvarsLen = length tvars

                  msg = "Type alias '" ++ name ++ "' expects " ++ show tvarsLen ++
                        " type argument" ++ (if tvarsLen == 1 then "" else "s") ++
                        " but was given " ++ show tsLen

replace :: Map.Map String T.CanonicalType -> T.CanonicalType -> T.CanonicalType
replace typeTable t =
    let go = replace typeTable in
    case t of
      T.Lambda a b ->
          T.Lambda (go a) (go b)

      T.Var x ->
          Map.findWithDefault t x typeTable

      T.Record fields ext ->
          T.Record (map (second go) fields) (fmap go ext)

      T.Aliased original t' ->
          T.Aliased original (go t')

      T.Data name ts ->
          T.Data name (map go ts)
