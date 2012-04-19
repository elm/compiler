module Replace (replace, depth) where

import Ast
import Data.Set (singleton,empty,unions,member)

replace y v expr =
    let f = replace y v in
    case expr of
      Range e1 e2 -> Range (f e1) (f e2)
      Access e x -> Access (f e) x
      Binop op e1 e2 -> Binop op (f e1) (f e2)
      App e1 e2 -> App (f e1) (f e2)
      If e1 e2 e3 -> If (f e1) (f e2) (f e3)
      Lift e es -> Lift (f e) (map f es)
      Fold e1 e2 e3 -> Fold (f e1) (f e2) (f e3)
      Async e -> Async (f e)
      Let defs e -> if elem y vs then Let defs e else Let (zip vs (map f es)) (f e)
              where (vs,es) = unzip defs
      Var x -> if x == y then v else Var x
      Data name es -> Data name (map f es)
      Case e cases -> Case (f e) $ map (case_replace y v) cases
      _ -> expr

case_replace y v (p,e) =
    if member y (pattern_vars p) then (p,e) else (p, replace y v e)

pattern_vars pattern =
    case pattern of
      PData _ ps -> unions (map pattern_vars ps)
      PVar x -> singleton x
      PAnything -> empty

depth = depth' 0
depth' d expr =
    let f = depth' (d+1) in
    case expr of
      Range e1 e2 -> max (f e1) (f e2)
      Access e x -> f e
      Binop op e1 e2 -> max (f e1) (f e2)
      Lambda x e -> f e
      App e1 e2 -> max (f e1) (f e2)
      If e1 e2 e3 -> maximum [f e1, f e2, f e3]
      Lift e es -> maximum $ f e : map f es
      Fold e1 e2 e3 -> maximum [f e1, f e2, f e3]
      Async e -> f e
      Let defs e -> let (_,es) = unzip defs in maximum $ f e : map f es
      Data "Cons" es -> maximum $ map (depth' d) es
      Data name es -> maximum $ 1 : map f es
      Case e cases -> maximum $ f e : map (f . snd) cases
      _ -> d
