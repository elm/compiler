{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Objects.Internal
  ( Graph(..)
  , fromModule
  , union
  , unions
  , SymbolTable
  , Symbol(..)
  , lookup
  , Roots
  , root
  , toGlobals
  )
  where


import Prelude hiding (lookup)
import Control.Arrow (first)
import Control.Monad (liftM, liftM2)
import Data.Binary
import qualified Data.Map as Map

import qualified AST.Expression.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified AST.Module as Module
import qualified AST.Variable as Var
import qualified Elm.Package as Pkg



-- OBJECT GRAPH


newtype Graph =
  Graph (Map.Map Var.Global Opt.Decl)



-- OBJECT GRAPH HELPERS


fromModule :: Module.Optimized -> Graph
fromModule (Module.Module home info) =
  Graph $ Map.fromList $
    map (first (Var.Global home)) (Module.program info)


union :: Graph -> Graph -> Graph
union (Graph objs1) (Graph objs2) =
  Graph (Map.union objs1 objs2)


unions :: [Graph] -> Graph
unions graphs =
  Graph (Map.unions (map destruct graphs))


destruct :: Graph -> Map.Map Var.Global Opt.Decl
destruct (Graph graph) =
  graph



-- SYMBOLS


data Symbol =
  Symbol
    { _home :: !Word16
    , _name :: !Word32
    }


newtype SymbolTable =
  SymbolTable (Map.Map Var.Global Symbol)


lookup :: Var.Global -> SymbolTable -> Maybe Symbol
lookup var (SymbolTable symbols) =
  Map.lookup var symbols



-- ROOTS


data Roots
  = Main ModuleName.Canonical


root :: Pkg.Name -> ModuleName.Raw -> Roots
root pkg name =
  Main (ModuleName.Canonical pkg name)


toGlobals :: Roots -> [Var.Global]
toGlobals roots =
  case roots of
    Main home ->
      [ Var.Global home "main" ]



-- BINARY


instance Binary Graph where
  put (Graph dict) =
    put dict

  get =
    liftM Graph get


instance Binary Symbol where
  put (Symbol home name) =
    put home >> put name

  get =
    liftM2 Symbol get get
