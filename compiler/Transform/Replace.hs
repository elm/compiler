module Replace (replace, depth) where

import Ast
import Control.Arrow ((***))
import Data.Set (singleton,empty,unions,member, Set)

replace :: String -> Expr -> Expr -> Expr
replace y v expr =
    let f = replace y v in
    case expr of
      Range e1 e2 -> Range (f e1) (f e2)
      Access e x -> Access (f e) x
      Binop op e1 e2 -> Binop op (f e1) (f e2)
      App e1 e2 -> App (f e1) (f e2)
      If e1 e2 e3 -> If (f e1) (f e2) (f e3)
      Guard ps -> map (f *** f) ps
      Lift e es -> Lift (f e) (map f es)
      Fold e1 e2 e3 -> Fold (f e1) (f e2) (f e3)
      Async e -> Async (f e)
      Let defs e -> if y `elem` vs then Let defs e else Let (zip vs (map f es)) (f e)
              where (vs,es) = unzip defs
      Var x -> if x == y then v else Var x
      ExplicitList es -> ExplicitList (map f es)
      Data name es -> Data name (map f es)
      Case e cases -> Case (f e) $ map (caseReplace y v) cases
      _ -> expr

caseReplace :: String -> Expr -> (Pattern, Expr) -> (Pattern, Expr)
caseReplace y v (p,e) =
    if member y (patternVars p) then (p,e) else (p, replace y v e)

patternVars :: Pattern -> Set String
patternVars pattern =
    case pattern of
      PData _ ps -> unions (map patternVars ps)
      PVar x -> singleton x
      PAsVar x p -> insert x (patternVars p)
      PAnything -> empty

depth :: Expr -> Integer
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
      Guard ps -> maximum (map (uncurry max . f *** f) ps)
      Lift e es -> maximum $ f e : map f es
      Fold e1 e2 e3 -> maximum [f e1, f e2, f e3]
      Async e -> f e
      Let defs e -> let (_,es) = unzip defs in maximum $ f e : map f es
      Data "::" es -> maximum $ map (depth' d) es
      Data name es -> maximum $ 1 : map f es
      Case e cases -> maximum $ f e : map (f . snd) cases
      _ -> d
