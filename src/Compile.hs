module Compile (compile) where

import qualified Data.List as List
import qualified Data.Map as Map

import qualified AST.Module as Module
import qualified Canonicalize
import Elm.Utils ((|>))
import qualified Nitpick.PatternMatches as Nitpick
import qualified Nitpick.TopLevelTypes as Nitpick
import qualified Parse.Helpers as Parse
import qualified Parse.Parse as Parse
import qualified Reporting.Error as Error
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning
import qualified Type.Inference as TI
import qualified Elm.Compiler.Package as Package


compile
    :: String
    -> String
    -> Bool
    -> Map.Map Module.Name Package.Name
    -> Module.Interfaces
    -> String
    -> Result.Result Warning.Warning Error.Error Module.CanonicalModule

compile user projectName isRoot pkgMap interfaces source =
  do
      -- determine if default imports should be added
      -- only elm-lang/core is exempt
      let needsDefaults =
            not (user == "elm-lang" && projectName == "core")

      -- Parse the source code
      validModule <-
          Result.mapError Error.Syntax $
            Parse.program needsDefaults isRoot (getOpTable interfaces) source

      --Omit interfaces from modules that we don't explicitly import
      --We don't need these for canonicalization, but we do need them for type checking
      let canonicalInterfaces =
            Map.mapWithKey (\nm -> List.filter (isImported pkgMap nm) ) interfaces

      -- Canonicalize all variables, pinning down where they came from.
      canonicalModule <-
          Canonicalize.module' canonicalInterfaces validModule

      -- Run type inference on the program.
      types <-
          Result.from Error.Type $
            TI.infer interfaces canonicalModule

      -- One last round of checks
      Result.mapError Error.Type $
        Nitpick.topLevelTypes types (Module.body validModule)

      --We can only pattern match against imported values
      --So we only give the imported interfaces
      Nitpick.patternMatches canonicalInterfaces canonicalModule

      -- Add the real list of types
      let body = (Module.body canonicalModule) { Module.types = types }

      return $ canonicalModule { Module.body = body }


getOpTable :: Module.Interfaces -> Parse.OpTable
getOpTable interfaces =
  (concat . Map.elems) interfaces
    |> concatMap Module.iFixities
    |> map (\(assoc,lvl,op) -> (op,(lvl,assoc)))
    |> Map.fromList


isImported
  :: Map.Map Module.Name Package.Name
  -> Module.Name
  -> Module.Interface
  -> Bool
isImported pkgMap nm iface =
  case Map.lookup nm pkgMap of
    Just x ->
      Module.iPackage iface == x
    Nothing ->
      error "Interface module name not in Package map"
