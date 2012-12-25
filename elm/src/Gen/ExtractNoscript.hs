
module ExtractNoscript (extractNoscript) where

import Ast
import Context
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

instance Extract e => Extract (Context e) where
  extract (C _ _ e) = extract e

instance Extract Expr where
  extract expr =
    let f = extract in
    case expr of
      Str s -> [s]
      Binop op e1 e2 -> case (op, f e1, f e2) of
                          ("++", [s1], [s2]) -> [s1 ++ s2]
                          (_   , ss1 , ss2 ) -> ss1 ++ ss2
      Lambda v e -> f e
      App (C _ _ (App (C _ _ (Var "link")) src)) txt -> linkExtract src txt
      App (C _ _ (App (C _ _ (Var "Graphics.link")) src)) txt -> linkExtract src txt
      App (C _ _ (App (C _ _ (Var "Text.link")) src)) txt -> linkExtract src txt
      App (C _ _ (Var "header")) e -> tag "h1" e
      App (C _ _ (Var "bold")) e -> tag "b" e
      App (C _ _ (Var "italic")) e -> tag "i" e
      App (C _ _ (Var "monospace")) e -> tag "code" e
      App e1 e2 -> f e1 ++ f e2
      If eb et ef -> f et ++ f ef
      Let defs e -> concatMap extract defs ++ f e
      Var _ -> []
      Case e cases -> concatMap (f . snd) cases
      Data _ es -> concatMap f es
      Markdown doc -> [ Pan.writeHtmlString Pan.defaultWriterOptions doc ]
      _ -> []

linkExtract src txt =
    case (extract src, extract txt) of
      ([s1],[s2]) -> [ "<a href=\"" ++ s1 ++ "\">" ++ s2 ++ "</a>" ]
      ( ss1, ss2) -> ss1 ++ ss2


tag t e = map (\s -> concat [ "<", t, ">", s, "</", t, ">" ]) (extract e)