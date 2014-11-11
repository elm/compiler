module Compile (compile) where

import qualified Data.Map as Map
import Text.PrettyPrint (Doc)

import qualified AST.Module as Module
import qualified Parse.Helpers as Parse
import qualified Parse.Parse as Parse
import qualified Transform.AddDefaultImports as DefaultImports
import qualified Transform.Check as Check
import qualified Type.Inference as TI
import qualified Transform.Canonicalize as Canonical
import Elm.Utils ((|>))


compile
    :: String
    -> String
    -> Module.Interfaces
    -> String
    -> Either [Doc] Module.CanonicalModule

compile user projectName interfaces source =
  do  
      -- Parse the source code
      parsedModule <-
        Parse.program (getOpTable interfaces) source

      -- determine if default imports should be added
      -- only elm-lang/core is exempt
      let needsDefaults =
            not (user == "elm-lang" && projectName == "core")

      -- add default imports if necessary
      let rawModule =
            DefaultImports.add needsDefaults parsedModule

      -- validate module (e.g. all variables exist)
      case Check.mistakes (Module.body rawModule) of
        [] -> return ()
        ms -> Left ms

      -- Canonicalize all variables, pinning down where they came from.
      canonicalModule <- Canonical.module' interfaces rawModule

      -- Run type inference on the program.
      types <- TI.infer interfaces canonicalModule

      -- Add the real list of tyes
      let body = (Module.body canonicalModule) { Module.types = types }

      return $ canonicalModule { Module.body = body }


getOpTable :: Module.Interfaces -> Parse.OpTable
getOpTable interfaces =
  Map.elems interfaces
    |> concatMap Module.iFixities
    |> map (\(assoc,lvl,op) -> (op,(lvl,assoc)))
    |> Map.fromList