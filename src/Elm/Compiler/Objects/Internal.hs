{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Objects.Internal
  ( Graph(..)
  , fromModule
  , union
  , unions
  , Roots
  , mains
  , value
  , toGlobals
  )
  where


import Prelude hiding (lookup)
import Control.Arrow (first)
import Control.Monad (liftM2)
import Data.Binary
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified AST.Expression.Optimized as Opt
import qualified AST.Kernel as Kernel
import qualified AST.Module.Name as ModuleName
import qualified AST.Module as Module
import qualified AST.Variable as Var



-- OBJECT GRAPH


data Graph =
  Graph
    { _decls :: Map.Map Var.Global Opt.Decl
    , _kernels :: Map.Map ModuleName.Raw Kernel.Data
    }



-- OBJECT GRAPH HELPERS


fromModule :: Module.Optimized -> Graph
fromModule (Module.Module home info) =
  let
    decls =
      Map.fromList $ map (first (Var.Global home)) (Module.program info)
  in
    Graph decls Map.empty


union :: Graph -> Graph -> Graph
union (Graph decls1 kernels1) (Graph decls2 kernels2) =
  Graph (Map.union decls1 decls2) (Map.union kernels1 kernels2)


unions :: [Graph] -> Graph
unions graphs =
  Graph
    (Map.unions (map _decls graphs))
    (Map.unions (map _kernels graphs))



-- ROOTS


data Roots
  = Mains [ModuleName.Canonical]
  | Value ModuleName.Canonical Text.Text


mains :: [ModuleName.Canonical] -> Roots
mains =
  Mains


value :: ModuleName.Canonical -> String -> Roots
value home name =
  Value home (Text.pack name)


toGlobals :: Roots -> [Var.Global]
toGlobals roots =
  case roots of
    Mains modules ->
      map (\home -> Var.Global home "main") modules

    Value home name ->
      [ Var.Global home name ]



-- BINARY


instance Binary Graph where
  put (Graph decls kernels) =
    put decls >> put kernels

  get =
    liftM2 Graph get get
