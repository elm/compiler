{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Optimize.Names
  ( Tracker
  , run
  , generate
  , registerKernel
  , registerGlobal
  , registerDebug
  , registerCtor
  , registerField
  , registerFieldDict
  , registerFieldList
  )
  where


import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A



-- GENERATOR


newtype Tracker a =
  Tracker (
    forall r.
      Int
      -> Set.Set Opt.Global
      -> Map.Map Name.Name Int
      -> (Int -> Set.Set Opt.Global -> Map.Map Name.Name Int -> a -> r)
      -> r
  )


run :: Tracker a -> (Set.Set Opt.Global, Map.Map Name.Name Int, a)
run (Tracker k) =
  k 0 Set.empty Map.empty
    (\_uid deps fields value -> (deps, fields, value))


generate :: Tracker Name.Name
generate =
  Tracker $ \uid deps fields ok ->
    ok (uid + 1) deps fields (Name.fromVarIndex uid)


registerKernel :: Name.Name -> a -> Tracker a
registerKernel home value =
  Tracker $ \uid deps fields ok ->
    ok uid (Set.insert (Opt.toKernelGlobal home) deps) fields value


registerGlobal :: ModuleName.Canonical -> Name.Name -> Tracker Opt.Expr
registerGlobal home name =
  Tracker $ \uid deps fields ok ->
    let global = Opt.Global home name in
    ok uid (Set.insert global deps) fields (Opt.VarGlobal global)


registerDebug :: Name.Name -> ModuleName.Canonical -> A.Region -> Tracker Opt.Expr
registerDebug name home region =
  Tracker $ \uid deps fields ok ->
    let global = Opt.Global ModuleName.debug name in
    ok uid (Set.insert global deps) fields (Opt.VarDebug name home region Nothing)


registerCtor :: ModuleName.Canonical -> Name.Name -> Index.ZeroBased -> Can.CtorOpts -> Tracker Opt.Expr
registerCtor home name index opts =
  Tracker $ \uid deps fields ok ->
    let
      global = Opt.Global home name
      newDeps = Set.insert global deps
    in
    case opts of
      Can.Normal ->
        ok uid newDeps fields (Opt.VarGlobal global)

      Can.Enum ->
        ok uid newDeps fields $
          case name of
            "True"  | home == ModuleName.basics -> Opt.Bool True
            "False" | home == ModuleName.basics -> Opt.Bool False
            _ -> Opt.VarEnum global index

      Can.Unbox ->
        ok uid (Set.insert identity newDeps) fields (Opt.VarBox global)


identity :: Opt.Global
identity =
  Opt.Global ModuleName.basics Name.identity


registerField :: Name.Name -> a -> Tracker a
registerField name value =
  Tracker $ \uid d fields ok ->
    ok uid d (Map.insertWith (+) name 1 fields) value


registerFieldDict :: Map.Map Name.Name v -> a -> Tracker a
registerFieldDict newFields value =
  Tracker $ \uid d fields ok ->
    ok uid d (Map.unionWith (+) fields (Map.map toOne newFields)) value


toOne :: a -> Int
toOne _ = 1


registerFieldList :: [Name.Name] -> a -> Tracker a
registerFieldList names value =
  Tracker $ \uid deps fields ok ->
    ok uid deps (foldr addOne fields names) value


addOne :: Name.Name -> Map.Map Name.Name Int -> Map.Map Name.Name Int
addOne name fields =
  Map.insertWith (+) name 1 fields



-- INSTANCES


instance Functor Tracker where
  fmap func (Tracker kv) =
    Tracker $ \n d f ok ->
      let
        ok1 n1 d1 f1 value =
          ok n1 d1 f1 (func value)
      in
      kv n d f ok1


instance Applicative Tracker where
  {-# INLINE pure #-}
  pure value =
    Tracker $ \n d f ok -> ok n d f value

  (<*>) (Tracker kf) (Tracker kv) =
    Tracker $ \n d f ok ->
      let
        ok1 n1 d1 f1 func =
          let
            ok2 n2 d2 f2 value =
              ok n2 d2 f2 (func value)
          in
          kv n1 d1 f1 ok2
      in
      kf n d f ok1


instance Monad Tracker where
  return = pure

  (>>=) (Tracker k) callback =
    Tracker $ \n d f ok ->
      let
        ok1 n1 d1 f1 a =
          case callback a of
            Tracker kb -> kb n1 d1 f1 ok
      in
      k n d f ok1
