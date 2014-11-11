module Build.Source (build) where

import qualified Data.Map as Map
import Text.PrettyPrint (Doc)

import qualified AST.Module as Module
import qualified Parse.Parse as Parse
import qualified Transform.AddDefaultImports as DefaultImports
import qualified Transform.Check as Check
import qualified Type.Inference as TI
import qualified Transform.Canonicalize as Canonical

build :: Bool -> Module.Interfaces -> String -> Either [Doc] Module.CanonicalModule
build noPrelude interfaces source =
  do let infixes = Map.fromList . map (\(assoc,lvl,op) -> (op,(lvl,assoc)))
                 . concatMap Module.iFixities $ Map.elems interfaces

     -- Parse the source code and validate that it is well formed.
     validModule <- do
       modul <- DefaultImports.add noPrelude `fmap` Parse.program infixes source
       case Check.mistakes (Module.body modul) of
         [] -> return modul
         ms -> Left ms

     -- Canonicalize all variables, pinning down where they came from.
     canonicalModule <- Canonical.module' interfaces validModule

     -- Run type inference on the program.
     types <- TI.infer interfaces canonicalModule

     -- Add the real list of tyes
     let body = (Module.body canonicalModule) { Module.types = types }

     return $ canonicalModule { Module.body = body }
