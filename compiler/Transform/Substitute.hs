module Substitute (subst) where

import Ast
import Located
import Control.Arrow (second, (***))

subst :: String -> Expr -> Expr -> Expr
subst old new expr =
    let f (L t s e) = L t s (subst old new e) in
    case expr of
      Range e1 e2 -> Range (f e1) (f e2)
      Access e x -> Access (f e) x
      Remove e x -> Remove (f e) x
      Insert e x v -> Insert (f e) x (f v)
      Modify r fs -> Modify (f r) (map (second f) fs)
      Record fs -> Record (map (\(lbl,as,e) -> (lbl,as,f e)) fs)
      Binop op e1 e2 -> Binop op (f e1) (f e2)
      Lambda x e -> if x == old then expr else Lambda x (f e)
      App e1 e2 -> App (f e1) (f e2)
      MultiIf ps -> MultiIf (map (f *** f) ps)
      Let defs e -> Let (map substDef defs) (f e)
              where substDef (FnDef name vs e)  = FnDef name vs (f e)
                    substDef (OpDef op a1 a2 e) = OpDef op a1 a2 (f e)
                    substDef x = x
      Var x -> if x == old then new else expr
      Case e cases -> Case (f e) $ map (second f) cases
      Data name es -> Data name (map f es)
      _ -> expr
