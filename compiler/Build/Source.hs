{-# OPTIONS_GHC -W #-}
module Build.Source (build) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import System.FilePath as FP
import Text.PrettyPrint (Doc)

import SourceSyntax.Declaration
import SourceSyntax.Module
import qualified SourceSyntax.Expression as Expr
import qualified SourceSyntax.Pattern as Pattern
import qualified SourceSyntax.Type as Type
import qualified Parse.Parse as Parse
import qualified Metadata.Prelude as Prelude
import qualified Transform.Check as Check
import qualified Transform.SortDefinitions as SD
import qualified Type.Inference as TI
import qualified Type.Constrain.Declaration as TcDecl
import qualified Transform.Canonicalize as Canonical

build :: Bool -> Interfaces -> String -> Either [Doc] MetadataModule
build noPrelude interfaces source =
  do let add = Prelude.add noPrelude
         infixes = Map.fromList . map (\(assoc,lvl,op) -> (op,(lvl,assoc)))
                 . concatMap iFixities $ Map.elems interfaces

     Module names exs ims decls <- do
           modul@(Module _ _ _ decls) <- add `fmap` Parse.program infixes source
           case Check.mistakes decls of
             [] -> return modul
             ms -> Left ms

     let exports'
             | null exs =
                 let get = Set.toList . Pattern.boundVars in
                 concat [ get pattern | Definition (Expr.Definition pattern _ _) <- decls ] ++
                 concat [ map fst ctors | Datatype _ _ ctors _ <- decls ] ++
                 [ name | TypeAlias name _ (Type.Record _ _) _ <- decls ]
             | otherwise = exs

     metaModule <- Canonical.metadataModule interfaces $
         MetadataModule
         { names = names
         , path = FP.joinPath names
         , exports = exports'
         , imports = ims
           -- reorder AST into strongly connected components
         , program = SD.sortDefs . Expr.dummyLet $ TcDecl.toExpr decls
         , types = Map.empty
         , datatypes = [ (name,vars,ctors,ds) | Datatype name vars ctors ds <- decls ]
         , fixities = [ (assoc,level,op) | Fixity assoc level op <- decls ]
         , aliases = [ (name,tvs,tipe,ds) | TypeAlias name tvs tipe ds <- decls ]
         }

     types <- TI.infer interfaces metaModule

     return $ metaModule { types = types }
