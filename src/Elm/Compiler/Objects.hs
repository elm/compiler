{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler.Objects
  ( Graph
  , union
  , unions
  , graphForPackage
  , Roots
  , mains
  , value
  )
  where


import qualified Data.Map as Map

import qualified AST.Module.Name as ModuleName
import qualified Elm.Compiler as Compiler
import Elm.Compiler.Objects.Internal as Obj



-- HELPER


graphForPackage :: Map.Map ModuleName.Raw Compiler.KernelInfo -> [Compiler.Result] -> Obj.Graph
graphForPackage kernels results =
  let
    kernelGraph =
      Obj.Graph Map.empty (Map.map (\(Compiler.KernelInfo info) -> info) kernels)
  in
    Obj.unions (kernelGraph : map Compiler._objs results)
