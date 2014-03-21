{-# OPTIONS_GHC -Wall #-}
module Transform.SafeNames (metadataModule) where

import Control.Arrow (first, (***))
import qualified Data.List as List
import qualified Data.Set as Set

import qualified Parse.Helpers as PHelp
import SourceSyntax.Annotation
import SourceSyntax.Expression
import qualified SourceSyntax.Helpers as SHelp
import SourceSyntax.Module
import qualified SourceSyntax.Pattern as P
import qualified SourceSyntax.Variable as Variable

var :: String -> String
var = List.intercalate "." . map (dereserve . deprime) . SHelp.splitDots
  where
    deprime = map (\c -> if c == '\'' then '$' else c)
    dereserve x = case Set.member x PHelp.jsReserveds of
                    False -> x
                    True  -> "$" ++ x

pattern :: P.Pattern -> P.Pattern
pattern pat =
    case pat of
      P.Var x -> P.Var (var x)
      P.Literal _ -> pat
      P.Record fs -> P.Record (map var fs)
      P.Anything -> pat
      P.Alias x p -> P.Alias (var x) (pattern p)
      P.Data name ps -> P.Data name (map pattern ps)

-- TODO: should be "normal expression" -> "expression for JS generation"
expression :: Expr -> Expr
expression (A ann expr) =
    let f = expression in
    A ann $
    case expr of
      Literal _ -> expr
      Var (Variable.Raw x) -> rawVar (var x)
      Range e1 e2 -> Range (f e1) (f e2)
      ExplicitList es -> ExplicitList (map f es)
      Binop op e1 e2 -> Binop op (f e1) (f e2)
      Lambda p e -> Lambda (pattern p) (f e)
      App e1 e2 -> App (f e1) (f e2)
      MultiIf ps -> MultiIf (map (f *** f) ps)
      Let defs body -> Let (map definition defs) (f body)
      Case e cases -> Case (f e) $ map (pattern *** f) cases
      Data name es -> Data name (map f es)
      Access e x -> Access (f e) (var x)
      Remove e x -> Remove (f e) (var x)
      Insert e x v -> Insert (f e) (var x) (f v)
      Modify r fs -> Modify (f r) (map (var *** f) fs)
      Record fs -> Record (map (var *** f) fs)
      Markdown uid md es -> Markdown uid md (map f es)
      PortIn name st -> PortIn name st
      PortOut name st signal -> PortOut name st (f signal)

definition :: Def -> Def
definition (Definition p e t) =
    Definition (pattern p) (expression e) t

metadataModule :: MetadataModule -> MetadataModule
metadataModule modul =
    modul
    { names = map var (names modul)
    , exports = map var (exports modul)
    , imports = map (first var) (imports modul)
    , program = expression (program modul)
    , aliases =
        let makeSafe (name,tvars,tipe) = (var name, tvars, tipe)
        in  map makeSafe (aliases modul)
    , datatypes =
        let makeSafe (name,tvars,ctors) = (var name, tvars, map (first var) ctors)
        in  map makeSafe (datatypes modul)
    }
