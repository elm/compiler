module Compile (compile) where

import qualified Data.Map as Map

import qualified AST.Module as Module
import qualified Canonicalize
import Elm.Utils ((|>))
import qualified Elm.Package as Package
import qualified Nitpick.PatternMatches as Nitpick
import qualified Nitpick.TopLevelTypes as Nitpick
import qualified Parse.Helpers as Parse
import qualified Parse.Parse as Parse
import qualified Reporting.Error as Error
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning
import qualified Type.Inference as TI


compile
    :: Map.Map Module.Name Package.Name
    -> Package.Name
    -> Bool
    -> Module.CanonicalInterfaces
    -> String
    -> Result.Result Warning.Warning Error.Error Module.CanonicalModule

compile importedPackages packageName isRoot interfaces source =
  do
      -- determine if default imports should be added
      -- only elm-lang/core is exempt
      let needsDefaults =
            not (packageName == Package.coreName)

      let normalInterfaces =
            unCanonicalizeInterfaces importedPackages interfaces 

      -- Parse the source code
      validModule <-
          Result.mapError Error.Syntax $
            Parse.program needsDefaults isRoot (getOpTable interfaces) source

      -- Canonicalize all variables, pinning down where they came from.
      canonicalModule <-
          Canonicalize.module' normalInterfaces  validModule

      -- Run type inference on the program.
      types <-
          Result.from Error.Type $
            TI.infer interfaces canonicalModule

      -- One last round of checks
      Result.mapError Error.Type $
        Nitpick.topLevelTypes types (Module.body validModule)

      Nitpick.patternMatches normalInterfaces canonicalModule

      -- Add the real list of types
      let body = (Module.body canonicalModule) { Module.types = types }

      return $ canonicalModule { Module.body = body }


getOpTable :: Module.CanonicalInterfaces -> Parse.OpTable
getOpTable interfaces =
  Map.elems interfaces
    |> concatMap Module.iFixities
    |> map (\(assoc,lvl,op) -> (op,(lvl,assoc)))
    |> Map.fromList


unCanonicalizeInterfaces
  :: Map.Map Module.Name Package.Name
  -> Module.CanonicalInterfaces
  -> Module.Interfaces
unCanonicalizeInterfaces packageInfo ifaces =
  let
    foldFun canonName iface mapSoFar =
      case Map.lookup (Module.canonModul canonName) packageInfo of
        Just pkg | pkg == Module.iPackage iface ->
          Map.insert (Module.canonModul canonName) iface mapSoFar

        _ ->
          mapSoFar
  in
   Map.foldrWithKey foldFun Map.empty ifaces 
