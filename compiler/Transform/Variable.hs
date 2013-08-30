{-# OPTIONS_GHC -Wall #-}
module Transform.Variable where

import Parse.Helpers (jsReserveds)
import SourceSyntax.Expression
import SourceSyntax.Location
import SourceSyntax.Module
import SourceSyntax.Pattern

import Data.Map (mapKeys)
import qualified Data.Set as Set
import Control.Arrow (second, (***))

-- Make all variable names acceptable JavaScript identifiers
makeVarsSafe :: MetadataModule t v -> MetadataModule t v
makeVarsSafe modul = modul {
  names = map makeSafe $ names modul
  , exports = map makeSafe $ exports modul
  , imports = map (makeSafe *** mapVarImports makeSafe) $ imports modul
  , program = mapVarExpr makeSafe $ program modul
  , types   = mapKeys makeSafe $ types modul
  , aliases = map (squish3 makeSafe (map makeSafe) id) $ aliases modul
  , datatypes = map (squish3 makeSafe (map makeSafe) (map (makeSafe *** id))) $ datatypes modul
  , foreignImports = map (squish4 id (mapVarExpr makeSafe) makeSafe id) $ foreignImports modul
  , foreignExports = map (squish3 id makeSafe id) $ foreignExports modul
  }
  where squish3 f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)
        squish4 f1 f2 f3 f4 (w, x, y, z) = (f1 w, f2 x, f3 y, f4 z)

makeSafe :: String -> String
makeSafe = dereserve . deprime
  where
    deprime = map (\c -> if c == '\'' then '$' else c)
    dereserve x = case Set.member x jsReserveds of
                    False -> x
                    True  -> "$" ++ x

-- Change the name of every variable
mapVarExpr :: (String -> String) -> LExpr t v -> LExpr t v
mapVarExpr f lexpr = rec lexpr
  where rec (L s e') = L s $ case e' of
          Var x -> Var (f x)
          Range e1 e2 -> Range (rec e1) (rec e2)
          ExplicitList es -> ExplicitList (map rec es)
          Binop op e1 e2 -> Binop op (rec e1) (rec e2)
          Lambda p e -> Lambda (fPat p) (rec e)
          App e1 e2 -> App (rec e1) (rec e2)
          MultiIf ps -> MultiIf (map (rec *** rec) ps)
          Let defs body -> Let (map recDef defs) (rec body)
            where recDef (Def name e)  = Def (fPat name) (rec e)
                  recDef anno@(TypeAnnotation _ _) = anno
          Case e cases -> Case (rec e) $ map (fPat *** rec) cases
          Data name es -> Data name (map rec es)
          Access e x -> Access (rec e) x
          Remove e x -> Remove (rec e) x
          Insert e x v -> Insert (rec e) x (rec v)
          Modify r fs -> Modify (rec r) (map (second rec) fs)
          Record fs -> Record (map (second rec) fs)
          Literal _ -> e'
          Markdown _ -> e'
        fPat = mapVarPat f

mapVarPat :: (String -> String) -> Pattern -> Pattern
mapVarPat f pat = rec pat
  where rec p' = case p' of
          PData s ps -> PData (f s) (map rec ps)
          PRecord ss -> PRecord ss
          PAlias s p -> PAlias (f s) (rec p)
          PVar s     -> PVar (f s)
          PAnything  -> p'
          PLiteral _ -> p'

mapVarImports :: (String -> String) -> ImportMethod -> ImportMethod
mapVarImports f im = case im of
  As s         -> As (f s)
  Importing ss -> Importing (map f ss)
  Hiding ss    -> Hiding (map f ss)
