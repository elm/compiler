{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Import
  ( Error(..)
  , Problem(..)
  , toReport
  )
  where


import qualified Data.Set as Set

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Reporting.Doc as D
import qualified Reporting.Render.Code as Code
import qualified Reporting.Report as Report
import qualified Reporting.Annotation as A



-- ERROR


data Error =
  Error A.Region ModuleName.Raw (Set.Set ModuleName.Raw) Problem


data Problem
  = NotFound
  | Ambiguous FilePath [FilePath] Pkg.Name [Pkg.Name]
  | AmbiguousLocal FilePath FilePath [FilePath]
  | AmbiguousForeign Pkg.Name Pkg.Name [Pkg.Name]



-- TO REPORT


toReport :: Code.Source -> Error -> Report.Report
toReport source (Error region name knownModules problem) =
  case problem of
    NotFound ->
      Report.Report "MODULE NOT FOUND" region [] $
        Report.toCodeSnippet source region Nothing
          (
            "You are trying to import a `" <> D.fromName name <> "` module:"
          ,
            error "TODO But I cannot find `Whatever` in your project or packages." knownModules
          )

    Ambiguous path paths pkg pkgs ->
      error "TODO Ambiguous" path paths pkg pkgs

    AmbiguousLocal path1 path2 paths ->
      error "TODO AmbiguousLocal" path1 path2 paths

    AmbiguousForeign pkg1 pkg2 pkgs ->
      error "TODO AmbiguousForeign" pkg1 pkg2 pkgs
