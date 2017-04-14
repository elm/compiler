{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Objects.Internal
  ( Graph(..)
  , fromModule
  , union
  , unions
  , SymbolTable
  , symbolTable
  , Symbol(..)
  , lookup
  , Roots
  , mains
  , value
  , toGlobals
  )
  where


import Prelude hiding (lookup)
import Control.Arrow (first)
import Control.Monad (liftM, liftM2)
import Data.Binary
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified AST.Expression.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified AST.Module as Module
import qualified AST.Variable as Var



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


newtype SymbolTable =
  SymbolTable (Map.Map Var.Global Symbol)


symbolTable :: Map.Map Var.Global Symbol -> SymbolTable
symbolTable =
  SymbolTable


data Symbol =
  Symbol
    { _home :: !Word16
    , _name :: !Word32
    }


lookup :: Var.Global -> SymbolTable -> Maybe Symbol
lookup var (SymbolTable symbols) =
  Map.lookup var symbols



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
  hiddenDependencies ++
    case roots of
      Mains modules ->
        map (\home -> Var.Global home "main") modules

      Value home name ->
        [ Var.Global home name ]


-- needed for (==) and toString
hiddenDependencies :: [Var.Global]
hiddenDependencies =
  [ Var.Global (ModuleName.inCore "Dict") "toList"
  , Var.Global (ModuleName.inCore "Set") "toList"
  ]



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
