module Compile (compile) where

import qualified Data.Map as Map

import qualified AST.Module as Module
import qualified Canonicalize
import Elm.Utils ((|>))
import qualified Nitpick.TopLevelTypes as Nitpick
import qualified Parse.Helpers as Parse
import qualified Parse.Parse as Parse
import qualified Reporting.Error as Error
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning
import qualified Type.Inference as TI


compile
    :: String
    -> String
    -> Bool
    -> Module.Interfaces
    -> String
    -> Result.Result Warning.Warning Error.Error Module.CanonicalModule

compile user projectName isRoot interfaces source =
  do
      -- determine if default imports should be added
      -- only elm-lang/core is exempt
      let needsDefaults =
            not (user == "elm-lang" && projectName == "core")

      -- Parse the source code
      validModule <-
          Result.mapError Error.Syntax $
            Parse.program needsDefaults isRoot (getOpTable interfaces) source

      -- Canonicalize all variables, pinning down where they came from.
      canonicalModule <-
          Canonicalize.module' interfaces validModule

      -- Run type inference on the program.
      types <-
          Result.from Error.Type $
            TI.infer interfaces canonicalModule

      -- One last round of checks
      Result.mapError Error.Type $
        Nitpick.topLevelTypes types (Module.body validModule)

      -- Add the real list of types
      let body = (Module.body canonicalModule) { Module.types = types }

      return $ canonicalModule { Module.body = body }


getOpTable :: Module.Interfaces -> Parse.OpTable
getOpTable interfaces =
  Map.elems interfaces
    |> concatMap Module.iFixities
    |> map (\(assoc,lvl,op) -> (op,(lvl,assoc)))
    |> Map.fromList
