module Reporting.Error.Import
  ( Error(..)
  , toReport
  )
  where


import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified File
import qualified Reporting.Error as Error
import qualified Reporting.Report as Report



-- ERROR


data Error
  = NotFound
  | Ambiguous FilePath [FilePath] Pkg.Name [Pkg.Name]
  | AmbiguousLocal FilePath FilePath [FilePath]
  | AmbiguousForeign Pkg.Name Pkg.Name [Pkg.Name]



-- TO REPORT


toReport :: Error -> Report.Report
toReport err =
  case err of
    NotFound ->
      error "TODO NotFound"

    Ambiguous path paths pkg pkgs ->
      error "TODO Ambiguous" path paths pkg pkgs

    AmbiguousLocal path1 path2 paths ->
      error "TODO AmbiguousLocal" path1 path2 paths

    AmbiguousForeign pkg1 pkg2 pkgs ->
      error "TODO AmbiguousForeign" pkg1 pkg2 pkgs
