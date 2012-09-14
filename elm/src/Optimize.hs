module Optimize (optimize) where

import Ast
import Control.Arrow (second)
import Data.Char (isAlpha)
import Substitute

optimize (Module name ims exs stmts) =
    Module name ims exs (map optimizeStmt stmts)

optimizeStmt stmt = if stmt == stmt' then stmt' else optimizeStmt stmt'
    where stmt' = simpStmt stmt
          simpStmt (Def name args e) = Def name args (simp e)
          simpStmt (ImportEvent js b elm t) = ImportEvent js (simp b) elm t
          simpStmt stmt = stmt

simp :: Expr -> Expr
simp expr =
    let f = simp in
    case expr of
      Range e1 e2 -> Range (f e1) (f e2)
      Binop op e1 e2 -> simp_binop op (f e1) (f e2)
      Lambda x e -> Lambda x (f e)
      App (Lambda x e1) e2 -> 
          if isValue e2' then subst x e2' e1' else App (Lambda x e1') e2'
              where { e1' = f e1 ; e2' = f e2 }
      App e1 e2 -> App (f e1) (f e2)
      If e1 e2 e3 -> simp_if (f e1) (f e2) (f e3)
      Let defs e -> Let (map simpDef defs) (f e)
              where simpDef (Definition func args e) = Definition func args (f e)
      Data name es -> Data name (map f es)
      Case e cases -> Case (f e) (map (second f) cases)
      _ -> expr

simp_if (Boolean b) e2 e3 = if b then e2 else e3
simp_if a b c = If a b c

isValue e = case e of { IntNum _  -> True
                      ; FloatNum _ -> True
                      ; Chr _ -> True
                      ; Str _ -> True
                      ; Boolean _ -> True
                      ; Var _ -> True
                      ; Data _ _ -> True
                      ; _ -> False }

simp_binop = binop

binop op (IntNum n) (IntNum m) = f n m
    where f a b = case op of
                    { "+" -> IntNum $ (+) a b
                    ; "-" -> IntNum $ (-) a b
                    ; "*" -> IntNum $ (*) a b
                    ; "div" -> IntNum $ div a b
                    ; "mod" -> IntNum $ mod a b
                    ; "<" -> Boolean $ a < b
                    ; ">" -> Boolean $ a < b
                    ; "<=" -> Boolean $ a <= b
                    ; ">=" -> Boolean $ a >= b
                    ; "==" -> Boolean $ a == b
                    ; "/=" -> Boolean $ a /= b
                    ;  _  -> Binop op (IntNum n) (IntNum m) }

binop "-" e (IntNum 0) = e
binop "+" (IntNum 0) e = e
binop "+" e (IntNum 0) = e
binop "+" (IntNum n) (Binop "+" (IntNum m) e) = binop "+" (IntNum (n+m)) e
binop "+" (IntNum n) (Binop "+" e (IntNum m)) = binop "+" (IntNum (n+m)) e

binop "/" e (IntNum 1) = e
binop "div" e (IntNum 1) = e
binop "*" (IntNum 0) e = IntNum 0
binop "*" (IntNum 1) e = e
binop "*" (IntNum n) (Binop "*" (IntNum m) e) = binop "*" (IntNum (n*m)) e
binop "*" (IntNum n) (Binop "*" e (IntNum m)) = binop "*" (IntNum (n*m)) e

binop "+" e (IntNum n) = binop "+" (IntNum n) e
binop "*" e (IntNum n) = binop "*" (IntNum n) e

binop op (Boolean n) (Boolean m) = f n m
    where f a b = case op of { "&&" -> Boolean $ (&&) n m
                             ; "||" -> Boolean $ (||) n m
                             ;  _  -> Binop op (Boolean n) (Boolean m) }

binop "&&" (Boolean  True) e = e
binop "&&" (Boolean False) e = Boolean False
binop "||" (Boolean  True) e = Boolean True
binop "||" (Boolean False) e = e

binop "&&" e (Boolean n) = binop "&&" (Boolean n) e
binop "||" e (Boolean n) = binop "||" (Boolean n) e

binop ":" h t = cons h t
binop "++" (Str s1) (Str s2) = Str $ s1 ++ s2
binop "++" (Str s1) (Binop "++" (Str s2) e) = Binop "++" (Str $ s1 ++ s2) e
binop "++" (Binop "++" e (Str s1)) (Str s2) = Binop "++" e (Str $ s1 ++ s2)
binop "++" (Data "Nil" []) e = e
binop "++" e (Data "Nil" []) = e
binop "++" (Data "Cons" [h,t]) e = Data "Cons" [h, binop "++" t e]

binop "$" e1 e2 = App e1 e2
binop "." e1 e2 = Lambda "x" (App e1 (App e2 (Var "x")))

binop op e1 e2
    | isAlpha (head op) || '_' == head op = App (App (Var op) e1) e2
    | otherwise = Binop op e1 e2