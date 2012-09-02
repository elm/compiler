module Substitute (subst) where

import Control.Arrow (second)

import Ast

subst :: String -> String -> Expr -> Expr
subst new old expr =
    let f = subst new old in
    case expr of
      Range e1 e2 -> Range (f e1) (f e2)
      Access e x -> Access (f e) x
      Binop op e1 e2 -> Binop op (f e1) (f e2)
      Lambda x e -> if x == old then expr else Lambda x (f e)
      App e1 e2 -> App (f e1) (f e2)
      If e1 e2 e3 -> If (f e1) (f e2) (f e3)
      Lift e es -> Lift (f e) (map f es)
      Fold e1 e2 e3 -> Fold (f e1) (f e2) (f e3)
      Async e -> Async (f e)
      Input _ -> expr
      Let defs e -> Let (map substDef defs) (f e)
              where substDef (Definition name vs e) = Definition name vs (f e)
      Var x -> if x == old then Var new else expr
      Case e cases -> Case (f e) $ map (second f) cases
      Data name es -> Data name (map f es)
      _ -> expr
