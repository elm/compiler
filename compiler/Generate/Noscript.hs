module Generate.Noscript (noscript) where

import Data.List (isInfixOf)
import qualified SourceSyntax.Declaration as D
import SourceSyntax.Expression
import SourceSyntax.Literal
import SourceSyntax.Location
import SourceSyntax.Module
import qualified Generate.Markdown as MD

noscript :: Extract def => Module def -> String
noscript modul = concat (extract modul)

class Extract a where
  extract :: a -> [String]

instance Extract def => Extract (Module def) where
  extract (Module _ _ _ stmts) =
      map (\s -> "<p>" ++ s ++ "</p>") (concatMap extract stmts)

instance Extract def => Extract (D.Declaration' port def) where
  extract (D.Definition d) = extract d
  extract _ = []

instance Extract Def where
  extract (Definition _ e _)   = extract e

instance Extract e => Extract (Located e) where
  extract (L _ e) = extract e

instance Extract def => Extract (Expr' def) where
  extract expr =
    let f = extract in
    case expr of
      Literal (Str s) -> [s]
      Binop op e1 e2 -> case (op, f e1, f e2) of
                          ("++", [s1], [s2]) -> [s1 ++ s2]
                          (_   , ss1 , ss2 ) -> ss1 ++ ss2
      Lambda v e -> f e
      App (L _ (App (L _ (App (L _ (Var func)) w)) h)) src
          | "image" `isInfixOf` func -> extractImage src
      App (L _ (App (L _ (Var func)) src)) txt
          | "link" `isInfixOf` func -> extractLink src txt
      App (L _ (Var func)) e
          | "header"    `isInfixOf` func -> tag "h1" e
          | "bold"      `isInfixOf` func -> tag "b" e
          | "italic"    `isInfixOf` func -> tag "i" e
          | "monospace" `isInfixOf` func -> tag "code" e
      App e1 e2 -> f e1 ++ f e2
      Let defs e -> concatMap extract defs ++ f e
      Var _ -> []
      Case e cases -> concatMap (f . snd) cases
      Data _ es -> concatMap f es
      MultiIf es -> concatMap (f . snd) es
      Markdown _ md _ -> [ MD.toHtml md ]
      _ -> []

extractLink src txt =
    case (extract src, extract txt) of
      ([s1],[s2]) -> [ "<a href=\"" ++ s1 ++ "\">" ++ s2 ++ "</a>" ]
      ( ss1, ss2) -> ss1 ++ ss2

extractImage src =
    case extract src of
      [s] -> ["<img src=\"" ++ s ++ "\">"]
      ss  -> ss

tag t e = map (\s -> concat [ "<", t, ">", s, "</", t, ">" ]) (extract e)