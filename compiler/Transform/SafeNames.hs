{-# OPTIONS_GHC -Wall #-}
module Transform.SafeNames (metadataModule) where

import Control.Arrow (first, (***))
import SourceSyntax.Expression
import SourceSyntax.Location
import SourceSyntax.Module
import SourceSyntax.Pattern
import qualified Data.Set as Set
import qualified Parse.Helpers as PHelp

var :: String -> String
var = dereserve . deprime
  where
    deprime = map (\c -> if c == '\'' then '$' else c)
    dereserve x = case Set.member x PHelp.jsReserveds of
                    False -> x
                    True  -> "$" ++ x

pattern :: Pattern -> Pattern
pattern pat =
    case pat of
      PVar x -> PVar (var x)
      PLiteral _ -> pat
      PRecord fs -> PRecord (map var fs)
      PAnything -> pat
      PAlias x p -> PAlias (var x) (pattern p)
      PData name ps -> PData name (map pattern ps)

expression :: LExpr -> LExpr
expression (L loc expr) =
    let f = expression in
    L loc $
    case expr of
      Literal _ -> expr
      Var x -> Var (var x)
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
      PortIn name st tt handler -> PortIn name st tt (f handler)
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
        let makeSafe (name,tvars,tipe,ds) = (var name, tvars, tipe, ds)
        in  map makeSafe (aliases modul)
    , datatypes =
        let makeSafe (name,tvars,ctors,ds) = (var name, tvars, map (first var) ctors, ds)
        in  map makeSafe (datatypes modul)
    }