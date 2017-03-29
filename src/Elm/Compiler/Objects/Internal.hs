{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Objects.Internal
  ( Graph(..)
  , fromModule
  , union
  , Roots
  , main
  , toGlobals
  )
  where


import Control.Arrow (first)
import Data.Binary
import qualified Data.Map as Map

import qualified AST.Expression.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified AST.Module as Module
import qualified AST.Variable as Var
import qualified Elm.Package as Pkg



-- OBJECTS


newtype Graph =
  Graph (Map.Map Var.Global Opt.Decl)



-- HELPERS


fromModule :: Module.Optimized -> Graph
fromModule (Module.Module home info) =
  Graph $ Map.fromList $
    map (first (Var.Global home)) (Module.program info)


union :: Graph -> Graph -> Graph
union (Graph objs1) (Graph objs2) =
  Graph (Map.union objs1 objs2)



-- ROOTS


data Roots
  = Main ModuleName.Canonical


main :: Pkg.Name -> ModuleName.Raw -> Roots
main pkg name =
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
    Graph <$> get
