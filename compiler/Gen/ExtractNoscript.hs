module ExtractNoscript (extractNoscript) where

import Ast
import Located
import qualified Text.Pandoc as Pan

extractNoscript :: Module -> String
extractNoscript modul = concat (extract modul)

class Extract a where
  extract :: a -> [String]

instance Extract Module where
  extract (Module _ _ _ stmts) =
      map (\s -> "<p>" ++ s ++ "</p>") (concatMap extract stmts)

instance Extract Statement where
  extract (Definition d) = extract d
  extract _ = []

instance Extract Def where
  extract (FnDef _ _ e)   = extract e
  extract (OpDef _ _ _ e) = extract e
  extract _ = []

instance Extract e => Extract (Located e) where
  extract (L _ _ e) = extract e

instance Extract Expr where
  extract expr =
    let f = extract in
    case expr of
      Str s -> [s]
      Binop op e1 e2 -> case (op, f e1, f e2) of
                          ("++", [s1], [s2]) -> [s1 ++ s2]
                          (_   , ss1 , ss2 ) -> ss1 ++ ss2
      Lambda v e -> f e
      App (L _ _ (App (L _ _ (Var "link")) src)) txt -> linkExtract src txt
      App (L _ _ (App (L _ _ (Var "Text.link")) src)) txt -> linkExtract src txt
      App (L _ _ (Var "header")) e -> tag "h1" e
      App (L _ _ (Var "bold")) e -> tag "b" e
      App (L _ _ (Var "italic")) e -> tag "i" e
      App (L _ _ (Var "monospace")) e -> tag "code" e
      App e1 e2 -> f e1 ++ f e2
      Let defs e -> concatMap extract defs ++ f e
      Var _ -> []
      Case e cases -> concatMap (f . snd) cases
      Data _ es -> concatMap f es
      MultiIf es -> concatMap (f . snd) es
      Markdown doc -> [ Pan.writeHtmlString Pan.def doc ]
      _ -> []

linkExtract src txt =
    case (extract src, extract txt) of
      ([s1],[s2]) -> [ "<a href=\"" ++ s1 ++ "\">" ++ s2 ++ "</a>" ]
      ( ss1, ss2) -> ss1 ++ ss2


tag t e = map (\s -> concat [ "<", t, ">", s, "</", t, ">" ]) (extract e)