{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Compile
  ( Artifacts(..)
  , compile
  )
  where


import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.OneOrMore as OneOrMore

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified Canonicalize.Module as Canonicalize
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Nitpick.PatternMatches as PatternMatches
import qualified Optimize.Module as Optimize
import qualified Reporting.Error as E
import qualified Reporting.Result as R
import qualified Reporting.Warning as W
import qualified Type.Constrain.Module as Type
import qualified Type.Solve as Type

import System.IO.Unsafe (unsafePerformIO)



-- COMPILE


data Artifacts =
  Artifacts
    { _modul :: Can.Module
    , _types :: Map.Map Name.Name Can.Annotation
    , _graph :: Opt.Graph
    }


compile :: Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> ([W.Warning], Either (OneOrMore.OneOrMore E.Error) Artifacts)
compile pkg ifaces modul =
  R.run $
  do  canonical   <- canonicalize pkg ifaces modul
      annotations <- typeCheck canonical
      ()          <- nitpick canonical
      objects     <- optimize annotations canonical
      return (Artifacts canonical annotations objects)



-- PHASES


canonicalize :: Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> R.Result i [W.Warning] E.Error Can.Module
canonicalize pkg ifaces modul =
  R.mapError E.Canonicalize $ Canonicalize.canonicalize pkg ifaces modul


typeCheck :: Can.Module -> R.Result i w E.Error (Map.Map Name.Name Can.Annotation)
typeCheck canonical =
  case unsafePerformIO (Type.run =<< Type.constrain canonical) of
    Right annotations ->
      R.ok annotations

    Left errors ->
      R.throw (E.Type errors)


nitpick :: Can.Module -> R.Result i w E.Error ()
nitpick canonical =
  case PatternMatches.check canonical of
    Right () ->
      R.ok ()

    Left errors ->
      R.throw (E.Pattern errors)


optimize :: Map.Map Name.Name Can.Annotation -> Can.Module -> R.Result i [W.Warning] E.Error Opt.Graph
optimize annotations canonical =
  R.mapError E.Main $ Optimize.optimize annotations canonical
