{-# OPTIONS_GHC -Wall #-}
module Elm.Project.Summary
  ( Summary(..)
  , ExposedModules
  , DepsGraph
  , init
  , cheapInit
  )
  where


import Prelude hiding (init)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Elm.Compiler.Module as Module
import Elm.Package (Package(..), Name, Version)
import Elm.Project.Json (Project(..), AppInfo(..), PkgInfo(..))
import qualified Elm.Project.Json as Project



-- SUMMARY


data Summary =
  Summary
    { _root :: FilePath
    , _project :: Project
    , _exposed :: ExposedModules
    , _ifaces :: Module.Interfaces
    , _depsGraph :: DepsGraph
    }


type ExposedModules =
  Map.Map Module.Raw [Package]


type DepsGraph =
  Map.Map Name ( Version, [Name] )



-- MAKE SUMMARY


init :: FilePath -> Project -> Map Name PkgInfo -> Module.Interfaces -> Summary
init root project deps ifaces =
  let
    exposed =
      case project of
        Project.App info ->
          getExposed deps (_app_deps_direct info)

        Project.Pkg info ->
          getExposed deps (_pkg_deps info)

    privatizedInterfaces =
      Map.mapMaybeWithKey (privatize exposed) ifaces

    depsGraph =
      Map.foldr toNode Map.empty deps
  in
    Summary root project exposed privatizedInterfaces depsGraph


privatize :: ExposedModules -> Module.Canonical -> Module.Interface -> Maybe Module.Interface
privatize exposed (Module.Canonical _ name) iface =
  case Map.lookup name exposed of
    Just [_] ->
      Just iface

    _ ->
      Nothing


toNode :: PkgInfo -> DepsGraph -> DepsGraph
toNode (PkgInfo name _ _ version _ deps _ _) graph =
  Map.insert name (version, Map.keys deps) graph



-- MAKE CHEAP SUMMARY


cheapInit :: FilePath -> PkgInfo -> Map Name PkgInfo -> Module.Interfaces -> Summary
cheapInit root info deps ifaces =
  Summary root (Pkg info) (getExposed deps (_pkg_deps info)) ifaces Map.empty


getExposed :: Map Name PkgInfo -> Map Name a -> ExposedModules
getExposed deps directs =
  Map.foldl insertExposed Map.empty (Map.intersection deps directs)


insertExposed :: ExposedModules -> PkgInfo -> ExposedModules
insertExposed exposed info =
  let
    home =
      Package (_pkg_name info) (_pkg_version info)

    insertModule dict modul =
      Map.insertWith (++) modul [home] dict
  in
    List.foldl' insertModule exposed (Project.getExposed info)
