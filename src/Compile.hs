module Compile (compile) where

import qualified Data.Map as Map

import qualified AST.Declaration as D
import qualified AST.Expression.Canonical as Can
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified Canonicalize
import Elm.Utils ((|>))
import qualified Elm.Package as Package
import qualified Nitpick.PatternMatches as Nitpick
import qualified Nitpick.TopLevelTypes as Nitpick
import qualified Optimize
import qualified Parse.Helpers as Parse
import qualified Parse.Parse as Parse
import qualified Reporting.Error as Error
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning
import qualified Type.Inference as TI



type Result =
  Result.Result (Result.One RenderType.Localizer) Warning.Warning Error.Error


compile
    :: Package.Name
    -> [ModuleName.Canonical]
    -> Module.Interfaces
    -> String
    -> Result Module.Optimized
compile packageName canonicalImports interfaces source =
  do
      -- Parse the source code
      validModule <-
          Result.format Error.Syntax $ {-# SCC parsing #-}
            Parse.program packageName (getOpTable interfaces) source

      -- Canonicalize all variables, pinning down where they came from.
      canonicalModule <-
          Canonicalize.module' canonicalImports interfaces validModule

      -- Run type inference on the program.
      types <-
          Result.from Error.Type $ {-# SCC type_inference #-}
            TI.infer interfaces canonicalModule

      -- One last round of checks
      canonicalDefs <-
          Result.format Error.Type $
            Nitpick.topLevelTypes types $
              Can.toSortedDefs (Module.program (Module.info canonicalModule))

      tagDict <-
        Result.format Error.Pattern $ {-# SCC exhaustiveness #-}
          Nitpick.patternMatches interfaces canonicalModule

      -- Do some basic optimizations
      let optimisedDefs = {-# SCC optimization #-}
            Optimize.optimize tagDict (Module.name canonicalModule) canonicalDefs

      -- Add the real list of types
      let info =
            (Module.info canonicalModule)
              { Module.types = types
              , Module.program = optimisedDefs
              }

      return $ canonicalModule { Module.info = info }


getOpTable :: Module.Interfaces -> Parse.OpTable
getOpTable interfaces =
  Map.elems interfaces
    |> concatMap Module.iFixities
    |> map (\(D.Infix op assoc lvl) -> ( op, (lvl,assoc) ))
    |> Map.fromList
