module Reporting.Error.Import
  ( Error(..)
  , toReport
  )
  where


import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Reporting.Report as Report



-- ERROR


data Error
  = NotFound ModuleName.Raw
  | Ambiguous ModuleName.Raw FilePath [FilePath] Pkg.Name [Pkg.Name]
  | AmbiguousLocal ModuleName.Raw FilePath FilePath [FilePath]
  | AmbiguousForeign ModuleName.Raw Pkg.Name Pkg.Name [Pkg.Name]



-- TO REPORT


toReport :: Error -> Report.Report
toReport err =
  case err of
    NotFound name ->
      error "TODO NotFound" name

    Ambiguous name path paths pkg pkgs ->
      error "TODO Ambiguous" name path paths pkg pkgs

    AmbiguousLocal name path1 path2 paths ->
      error "TODO AmbiguousLocal" name path1 path2 paths

    AmbiguousForeign name pkg1 pkg2 pkgs ->
      error "TODO AmbiguousForeign" name pkg1 pkg2 pkgs


{-

-- MODULE NOT FOUND ---------------------------------------------- src/Main.elm

You are trying to import a `Whatever` module:

  import Whatever

But I cannot find `Whatever` in your project or packages.

-}
