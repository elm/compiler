
module ExtractNoscript (extract) where

import Ast
import List (isPrefixOf)

extract = extract'

extract' expr = makeLinks terms where
    terms =
        case expr of
          Number _ -> []
          Chr _ -> []
          Boolean _ -> []
          Range _ _ -> []
          Access _ _ -> []
          Binop op e1 e2 -> case (op, extract' e1, extract' e2) of
                              ("++", [s1], [s2]) -> [ s1 ++ s2 ]
                              (_   , ss1 , ss2 ) -> ss1 ++ ss2
          Lambda v e -> extract' e
          App e1 e2 -> extract' e1 ++ extract' e2
          If eb et ef -> extract' et ++ extract' ef
          Lift f es -> extract' f ++ concatMap extract es
          Fold e1 e2 e3 -> concatMap extract' [e1,e2,e3]
          Async e -> extract' e
          Input _ -> []
          Let defs e -> concatMap (extract' . snd) defs ++ extract' e
          Var _ -> []
          Case e cases -> concatMap (extract' . snd) cases
          Data _ _ -> [toString expr]

toString (Data "Cons" [Chr c, t]) = c : toString t
toString _ = []

makeLinks (a:b:[]) = linkify a b
makeLinks xs       = xs

linkish s = not (isPrefixOf "<a" s) && all (/=' ') s && any (=='/') s

linkify a b
    | linkish a = [ "<a href=\"" ++ a ++ "\">" ++ b ++ "</a>" ]
    | linkish b = [ "<a href=\"" ++ b ++ "\">" ++ a ++ "</a>" ]
    | otherwise = [a,b]