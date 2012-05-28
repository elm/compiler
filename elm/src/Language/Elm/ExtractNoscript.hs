
module Language.Elm.ExtractNoscript (extract) where

import Ast

extract = concatMap (\s -> "<p>" ++ s ++ "</p>") . extract'

extract' expr = 
    let f = extract' in
    case expr of
      Str s -> [s]
      Binop op e1 e2 -> case (op, f e1, f e2) of
                          ("++", [s1], [s2]) -> [s1 ++ s2]
                          (_   , ss1 , ss2 ) -> ss1 ++ ss2
      Lambda v e -> f e
      App (App (Var "link") src) txt ->
          case (f src, f txt) of
            ([s1],[s2]) -> [ "<a href=\"" ++ s1 ++ "\">" ++ s2 ++ "</a>" ]
            ( ss1, ss2) -> ss1 ++ ss2
      App e1 e2 -> f e1 ++ f e2
      If eb et ef -> f et ++ f ef
      Let defs e -> concatMap (f . snd) defs ++ f e
      Var _ -> []
      Case e cases -> concatMap (f . snd) cases
      Data _ es -> concatMap f es
      _ -> []