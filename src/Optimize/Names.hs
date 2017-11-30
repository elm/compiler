{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Optimize.Names
  ( Tracker
  , run
  , generate
  , registerKernel
  , registerGlobal
  )
  where


import Data.Monoid ((<>))
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
      -> (Int -> Set.Set N.Name -> Set.Set Opt.Global -> a -> r)
      -> r
  )


run :: Tracker a -> (Set.Set N.Name, Set.Set Opt.Global, a)
run (Tracker k) =
  k 0 Set.empty Set.empty
    (\_uid kernels globals value -> (kernels, globals, value))


generate :: Tracker N.Name
generate =
  Tracker $ \n k g ok ->
    ok (n + 1) k g ("_n" <> Text.pack (show n))


registerKernel :: N.Name -> a -> Tracker a
registerKernel name value =
  Tracker $ \n k g ok ->
    ok n (Set.insert name k) g value


registerGlobal :: ModuleName.Canonical -> N.Name -> Tracker Opt.Expr
registerGlobal home name =
  Tracker $ \n k g ok ->
    let global = Opt.Global home name in
    ok n k (Set.insert global g) (Opt.VarGlobal global)



-- INSTANCES


instance Functor Tracker where
  fmap func (Tracker kv) =
    Tracker $ \n k g ok ->
      let
        ok1 n1 k1 g1 value =
          ok n1 k1 g1 (func value)
      in
      kv n k g ok1


instance Applicative Tracker where
  {-# INLINE pure #-}
  pure value =
    Tracker $ \n k g ok -> ok n k g value

  (<*>) (Tracker kf) (Tracker kv) =
    Tracker $ \n0 k0 g0 ok ->
      let
        ok1 n1 k1 g1 func =
          let
            ok2 n2 k2 g2 value =
              ok n2 k2 g2 (func value)
          in
          kv n1 k1 g1 ok2
      in
      kf n0 k0 g0 ok1


instance Monad Tracker where
  return = pure

  (>>=) (Tracker ka) callback =
    Tracker $ \n k g ok ->
      let
        ok1 n1 k1 g1 a =
          case callback a of
            Tracker kb -> kb n1 k1 g1 ok
      in
      ka n k g ok1
