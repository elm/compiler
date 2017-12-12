{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler.Objects
  ( JS.Mode(..)
  , JS.generate
  , JS.Roots
  , mains
  , value
  , Opt.Graph
  , empty
  , union
  , unions
  ,
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Generate.JavaScript as JS



-- ROOTS


mains :: [ModuleName.Canonical] -> JS.Roots
mains =
  JS.Mains


value :: ModuleName.Canonical -> String -> JS.Roots
value home name =
  JS.Value home (Text.pack name)



-- COMBINE GRAPHS


{-# NOINLINE empty #-}
empty :: Opt.Graph
empty =
  Opt.Graph Map.empty Map.empty Map.empty


union :: Opt.Graph -> Opt.Graph -> Opt.Graph
union (Opt.Graph mains1 graph1 fields1) (Opt.Graph mains2 graph2 fields2) =
  Opt.Graph
    (Map.union mains1 mains2)
    (Map.union graph1 graph2)
    (Map.union fields1 fields2)


unions :: [Opt.Graph] -> Opt.Graph
unions graphs =
  List.foldl' union empty graphs
