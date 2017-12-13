{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler.Objects
  ( JS.Mode(..)
  , Name.Target(..)
  , JS.generate
  , JS.Roots
  , mains
  , value
  , Opt.Graph
  , empty
  , union
  , Kernel(..)
  , fromKernels
  )
  where


import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Generate.JavaScript as JS
import qualified Generate.JavaScript.Name as Name



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



-- KERNEL GRAPHS


data Kernel =
  Kernel
    { _client :: Opt.KContent
    , _server :: Maybe Opt.KContent
    }


fromKernels :: Map.Map N.Name Kernel -> Opt.Graph
fromKernels kernels =
  Opt.Graph
    Map.empty
    (Map.mapKeys toGlobal (Map.map toNode kernels))
    Map.empty


toGlobal :: N.Name -> Opt.Global
toGlobal home =
  Opt.Global (ModuleName.Canonical Pkg.kernel home) N.dollar


toNode :: Kernel -> Opt.Node
toNode (Kernel client server) =
  Opt.Kernel client server
