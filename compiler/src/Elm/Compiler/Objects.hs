{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler.Objects
  ( JS.Output(..)
  , JS.generate
  , JS.generateForRepl
  , Opt.Graph
  , empty
  , union
  , unions
  , Kernel(..)
  , fromKernels
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name

import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Elm.Package as Pkg
import qualified Generate.JavaScript as JS



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
  case graphs of
    [] ->
      empty

    g:gs ->
      List.foldl' union g gs



-- KERNEL GRAPHS


data Kernel =
  Kernel
    { _client :: Opt.KContent
    , _server :: Maybe Opt.KContent
    }


fromKernels :: Map.Map Name.Name Kernel -> Opt.Graph
fromKernels kernels =
  Opt.Graph
    Map.empty
    (Map.mapKeys toGlobal (Map.map toNode kernels))
    (Map.foldl' addKernel Map.empty kernels)



-- KERNEL TO NODES


toGlobal :: Name.Name -> Opt.Global
toGlobal home =
  Opt.Global (ModuleName.Canonical Pkg.kernel (Name.getKernel home)) Name.dollar


toNode :: Kernel -> Opt.Node
toNode (Kernel client server) =
  Opt.Kernel client server



-- KERNEL TO ELM FIELDS


addKernel :: Map.Map Name.Name Int -> Kernel -> Map.Map Name.Name Int
addKernel fields (Kernel client maybeServer) =
  addContent (maybe fields (addContent fields) maybeServer) client


addContent :: Map.Map Name.Name Int -> Opt.KContent -> Map.Map Name.Name Int
addContent fields (Opt.KContent chunks _) =
  List.foldl' addChunk fields chunks


addChunk :: Map.Map Name.Name Int -> Opt.KChunk -> Map.Map Name.Name Int
addChunk fields chunk =
  case chunk of
    Opt.ElmField name ->
      Map.insertWith (+) name 1 fields

    _ ->
      fields
