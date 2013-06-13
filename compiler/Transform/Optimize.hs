module Optimize (optimize) where

import Ast
import Located
import Control.Arrow (second, (***))
import Data.Char (isAlpha)
import Substitute

optimize (Module name ims exs stmts) =
    Module name ims exs (map optimizeStmt stmts)

optimizeStmt stmt = if stmt == stmt' then stmt' else optimizeStmt stmt'
    where stmt' = simp stmt

class Simplify a where
  simp :: a -> a

instance Simplify Statement where
  simp (Definition def) = Definition (simp def)
  simp (ImportEvent js b elm t) = ImportEvent js (simp b) elm t
  simp stmt = stmt

instance Simplify Def where
  simp (FnDef func args e) = FnDef func args (simp e)
  simp (OpDef op a1 a2 e)  = OpDef op a1 a2 (simp e)
  simp x = x

instance Simplify e => Simplify (Located e) where
  simp (L t s e) = L t s (simp e)

instance Simplify Expr where
  simp expr =
    let f = simp in
    case expr of
      Range e1 e2 -> Range (f e1) (f e2)
      Binop op e1 e2 -> simp_binop op (f e1) (f e2)
      Lambda x e -> Lambda x (f e)
      Record fs -> Record (map (\(f,as,e) -> (f, as, simp e)) fs)
      App (L t s (Lambda x e1)) e2 -> 
        if isValue e2' then subst x e2' e1' else App (L t s (Lambda x ce1')) ce2'
              where ce1'@(L _ _ e1') = f e1
                    ce2'@(L _ _ e2') = f e2
      App e1 e2 -> App (f e1) (f e2)
      Let defs e -> Let (map simp defs) (f e)
      Data name es -> Data name (map f es)
      MultiIf es -> MultiIf . clipBranches $ map (f *** f) es
      Case e cases -> Case (f e) (map (second f) cases)
      _ -> expr


clipBranches [] = []
clipBranches (e:es) =
    case e of
      (L _ _ (Boolean True), _) -> [e]
      _ -> e : clipBranches es


isValue e =
    case e of { IntNum _  -> True
              ; FloatNum _ -> True
              ; Chr _ -> True
              ; Str _ -> True
              ; Boolean _ -> True
              ; Var _ -> True
              ; Data _ _ -> True
              ; _ -> False }

simp_binop = binop

binop op ce1@(L t1 s1 e1) ce2@(L t2 s2 e2) =
  let c1 = L t1 s1 in
  let c2 = L t2 s2 in
  case (op, e1, e2) of
    (_, IntNum n, IntNum m) -> case op of
                                 { "+" -> IntNum $ (+) n m
                                 ; "-" -> IntNum $ (-) n m
                                 ; "*" -> IntNum $ (*) n m
                                 ; "^" -> IntNum $ n ^ m
                                 ; "div" -> IntNum $ div n m
                                 ; "mod" -> IntNum $ mod n m
                                 ; "<" -> Boolean $ n < m
                                 ; ">" -> Boolean $ n < m
                                 ; "<=" -> Boolean $ n <= m
                                 ; ">=" -> Boolean $ n >= m
                                 ; "==" -> Boolean $ n == m
                                 ; "/=" -> Boolean $ n /= m
                                 ;  _  -> Binop op ce1 ce2 }

    -- flip order to move lone integers to the left
    ("+", _, IntNum n) -> binop "+" ce2 ce1
    ("*", _, IntNum n) -> binop "*" ce2 ce1

    ("+", IntNum 0, _) -> e2
    ("+", IntNum n, Binop "+" (L _ _ (IntNum m)) ce) ->
        binop "+" (c1 $ IntNum (n+m)) ce
    ("+", Binop "+" (L _ _ (IntNum n)) ce1'
        , Binop "+" (L _ _ (IntNum m)) ce2') ->
        binop "+" (notLocated $ IntNum (n+m)) (notLocated $ Binop "+" ce1' ce2')

    ("*", IntNum 0, _) -> e1
    ("*", IntNum 1, _) -> e2
    ("*", IntNum n, Binop "*" (L _ _ (IntNum m)) ce) ->
        binop "*" (notLocated $ IntNum (n*m)) ce
    ("*", Binop "*" (L _ _ (IntNum n)) ce1'
        , Binop "*" (L _ _ (IntNum m)) ce2') ->
        binop "*" (notLocated $ IntNum (n*m)) (notLocated $ Binop "*" ce1' ce2')

    ("-", _, IntNum 0) -> e1
    ("/", _, IntNum 1) -> e1
    ("div", _, IntNum 1) -> e1

    (_, Boolean n, Boolean m) -> case op of "&&" -> Boolean $ n && m
                                            "||" -> Boolean $ n || m
                                            _    -> Binop op ce1 ce2

    ("&&", Boolean  True, _) -> e2
    ("&&", Boolean False, _) -> Boolean False
    ("||", Boolean  True, _) -> Boolean True
    ("||", Boolean False, _) -> e2

    ("::", _, _) -> let (L _ _ e) = cons ce1 ce2 in e

    ("++", Str s1, Str s2) -> Str $ s1 ++ s2
    ("++", Str s1, Binop "++" (L _ _ (Str s2)) ce) ->
        Binop "++" (c1 $ Str $ s1 ++ s2) ce
    ("++", Binop "++" e (L _ _ (Str s1)), Str s2) ->
        Binop "++" e (c1 $ Str $ s1 ++ s2)

    ("++", Data "Nil" [], _) -> e2
    ("++", _, Data "Nil" []) -> e1
    ("++", Data "Cons" [h,t], _) -> Data "Cons" [h, notLocated $ binop "++" t ce2]

    ("|>", _, _) -> App ce2 ce1
    ("<|", _, _) -> App ce1 ce2
    (".", _, _) ->
        Lambda "x" (notLocated $
                      App ce1 (notLocated $ App ce2 (notLocated $ Var "x")))

    _ | isAlpha (head op) || '_' == head op ->
          App (notLocated $ App (notLocated $ Var op) ce1) ce2
      | otherwise -> Binop op ce1 ce2
