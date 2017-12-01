{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Optimize.Names
  ( Tracker
  , run
  , generate
  , registerKernel
  , registerGlobal
  , registerFields
  , registerField
  )
  where


import Data.Monoid ((<>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N



-- GENERATOR


newtype Tracker a =
  Tracker (
    forall r.
      Int
      -> Set.Set N.Name
      -> Set.Set Opt.Global
      -> Map.Map N.Name Int
      -> (Int -> Set.Set N.Name -> Set.Set Opt.Global -> Map.Map N.Name Int -> a -> r)
      -> r
  )


run :: Tracker a -> (Set.Set N.Name, Set.Set Opt.Global, Map.Map N.Name Int, a)
run (Tracker k) =
  k 0 Set.empty Set.empty Map.empty
    (\_uid kernels globals fields value -> (kernels, globals, fields, value))


generate :: Tracker N.Name
generate =
  Tracker $ \n k g f ok ->
    ok (n + 1) k g f ("_n" <> Text.pack (show n))


registerKernel :: N.Name -> a -> Tracker a
registerKernel name value =
  Tracker $ \n k g f ok ->
    ok n (Set.insert name k) g f value


registerGlobal :: ModuleName.Canonical -> N.Name -> Tracker Opt.Expr
registerGlobal home name =
  Tracker $ \n k g f ok ->
    let global = Opt.Global home name in
    ok n k (Set.insert global g) f (Opt.VarGlobal global)


registerField :: N.Name -> a -> Tracker a
registerField name value =
  Tracker $ \n k g fields ok ->
    ok n k g (Map.insertWith (+) name 1 fields) value


registerFields :: Map.Map N.Name v -> a -> Tracker a
registerFields newFields value =
  Tracker $ \n k g fields ok ->
    ok n k g (Map.unionWith (+) fields (Map.map toOne newFields)) value


toOne :: a -> Int
toOne _ = 1



-- INSTANCES


instance Functor Tracker where
  fmap func (Tracker kv) =
    Tracker $ \n k g f ok ->
      let
        ok1 n1 k1 g1 f1 value =
          ok n1 k1 g1 f1 (func value)
      in
      kv n k g f ok1


instance Applicative Tracker where
  {-# INLINE pure #-}
  pure value =
    Tracker $ \n k g f ok -> ok n k g f value

  (<*>) (Tracker kf) (Tracker kv) =
    Tracker $ \n0 k0 g0 f0 ok ->
      let
        ok1 n1 k1 g1 f1 func =
          let
            ok2 n2 k2 g2 f2 value =
              ok n2 k2 g2 f2 (func value)
          in
          kv n1 k1 g1 f1 ok2
      in
      kf n0 k0 g0 f0 ok1


instance Monad Tracker where
  return = pure

  (>>=) (Tracker ka) callback =
    Tracker $ \n k g f ok ->
      let
        ok1 n1 k1 g1 f1 a =
          case callback a of
            Tracker kb -> kb n1 k1 g1 f1 ok
      in
      ka n k g f ok1
