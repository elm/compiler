module Optimize (optimize) where

import Ast
import Control.Arrow (second)
import Data.Char (isAlpha)


optimize exp = if exp == exp' then exp' else optimize exp'
    where exp' = simp exp

simp expr =
    let f = simp in
    case expr of
      Range e1 e2 -> Range (f e1) (f e2)
      Binop op e1 e2 -> simp_binop op (f e1) (f e2)
      Lambda x e -> Lambda x (f e)
      App e1 e2 -> App (f e1) (f e2)
      If e1 e2 e3 -> simp_if (f e1) (f e2) (f e3)
      Let defs e -> Let (map (second f) defs) (f e)
      Data name es -> Data name (map f es)
      Case e cases -> Case (f e) (map (second f) cases)
      _ -> expr

simp_if (Boolean b) e2 e3 = if b then e2 else e3
simp_if a b c = If a b c

simp_binop "mod" (IntNum n) (IntNum m) = IntNum (mod n m)
simp_binop "mod" e1 e2 = Binop "mod" e1 e2
simp_binop str e1 e2
    | isAlpha (head str) || '_' == head str = App (App (Var str) e1) e2
    | otherwise = binop str e1 e2

binop op (IntNum n) (IntNum m) = f n m
    where f a b = case op of
                    { "+" -> IntNum $ (+) a b
                    ; "-" -> IntNum $ (-) a b
                    ; "*" -> IntNum $ (*) a b
                    ; "div" -> IntNum $ div a b
                    ; "<" -> Boolean $ a < b
                    ; ">" -> Boolean $ a < b
                    ; "<=" -> Boolean $ a <= b
                    ; ">=" -> Boolean $ a >= b
                    ; "==" -> Boolean $ a == b
                    ; "/=" -> Boolean $ a /= b
                    ;  _  -> Binop op (IntNum n) (IntNum m) }

binop "-" e (IntNum 0) = e
binop "+" (IntNum 0) e = e
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

binop op e (Boolean n) = binop op (Boolean n) e

binop ":" h t = cons h t
binop "++" (Str s1) (Str s2) = Str $ s1 ++ s2
binop "++" (Str s1) (Binop "++" (Str s2) e) = Binop "++" (Str $ s1 ++ s2) e
binop "++" (Binop "++" e (Str s1)) (Str s2) = Binop "++" e (Str $ s1 ++ s2)
binop "++" (Data "Nil" []) e = e
binop "++" e (Data "Nil" []) = e
binop "++" (Data "Cons" [h,t]) e = Data "Cons" [h, binop "++" t e]

binop "$" e1 e2 = App e1 e2

binop op e1 e2 = Binop op e1 e2
--}