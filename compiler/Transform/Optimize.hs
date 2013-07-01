module Transform.Optimize (optimize) where

import SourceSyntax.Everything
import Control.Arrow (second, (***))
import Data.Char (isAlpha)
import Data.Data
import Transform.Substitute

optimize (Module name ims exs stmts) =
    Module name ims exs (map optimizeStmt stmts)

optimizeStmt stmt = if stmt == stmt' then stmt' else optimizeStmt stmt'
    where stmt' = simp stmt

class Simplify a where
  simp :: a -> a

instance (Data t, Data v) => Simplify (Declaration t v) where
  simp (Definition def) = Definition (simp def)
  simp (ImportEvent js b elm t) = ImportEvent js (simp b) elm t
  simp stmt = stmt

instance (Data t, Data v) => Simplify (Def t v) where
  simp (FnDef func args e) = FnDef func args (simp e)
  simp (OpDef op a1 a2 e)  = OpDef op a1 a2 (simp e)
  simp x = x

instance Simplify e => Simplify (Located e) where
  simp (L t s e) = L t s (simp e)

instance (Data t, Data v) => Simplify (Expr t v) where
  simp expr =
    let f = simp in
    case expr of
      Range e1 e2 -> Range (f e1) (f e2)
      Binop op e1 e2 -> binop op (f e1) (f e2)
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
      (L _ _ (Literal (Boolean True)), _) -> [e]
      _ -> e : clipBranches es


isValue e =
    case e of
      Literal _  -> True
      Var _      -> True
      Data _ _   -> True
      _          -> False


binop op ce1@(L t1 s1 e1) ce2@(L t2 s2 e2) =
  let c1 = L t1 s1
      c2 = L t2 s2
      int = Literal . IntNum
      str = Literal . Str
      bool = Literal . Boolean
  in  
  case (op, e1, e2) of
    (_, Literal (IntNum n), Literal (IntNum m)) ->
        case op of
          { "+" -> int $ (+) n m
          ; "-" -> int $ (-) n m
          ; "*" -> int $ (*) n m
          ; "^" -> int $ n ^ m
          ; "div" -> int $ div n m
          ; "mod" -> int $ mod n m
          ; "rem" -> int $ rem n m
          ; "<" -> bool $ n < m
          ; ">" -> bool $ n < m
          ; "<=" -> bool $ n <= m
          ; ">=" -> bool $ n >= m
          ; "==" -> bool $ n == m
          ; "/=" -> bool $ n /= m
          ;  _  -> Binop op ce1 ce2 }
  {--
    -- flip order to move lone integers to the left
    ("+", _, IntNum n) -> binop "+" ce2 ce1
    ("*", _, IntNum n) -> binop "*" ce2 ce1

    ("+", IntNum 0, _) -> e2
    ("+", IntNum n, Binop "+" (L _ _ (IntNum m)) ce) ->
        binop "+" (c1 $ IntNum (n+m)) ce
    ("+", Binop "+" (L _ _ (IntNum n)) ce1'
        , Binop "+" (L _ _ (IntNum m)) ce2') ->
        binop "+" (none $ IntNum (n+m)) (none $ Binop "+" ce1' ce2')

    ("*", IntNum 0, _) -> e1
    ("*", IntNum 1, _) -> e2
    ("*", IntNum n, Binop "*" (L _ _ (IntNum m)) ce) ->
        binop "*" (none $ IntNum (n*m)) ce
    ("*", Binop "*" (L _ _ (IntNum n)) ce1'
        , Binop "*" (L _ _ (IntNum m)) ce2') ->
        binop "*" (none $ IntNum (n*m)) (none $ Binop "*" ce1' ce2')

    ("-", _, IntNum 0) -> e1
    ("/", _, IntNum 1) -> e1
    ("div", _, IntNum 1) -> e1
--}
    (_, Literal (Boolean n), Literal (Boolean m)) ->
        case op of
          "&&" -> bool $ n && m
          "||" -> bool $ n || m
          _    -> Binop op ce1 ce2

    ("&&", Literal (Boolean  True), _) -> e2
    ("&&", Literal (Boolean False), _) -> bool False
    ("||", Literal (Boolean  True), _) -> bool True
    ("||", Literal (Boolean False), _) -> e2

    ("::", _, _) -> Data "::" [ce1, ce2]

    ("++", Literal (Str s1), Literal (Str s2)) -> str $ s1 ++ s2
    ("++", Literal (Str s1), Binop "++" (L _ _ (Literal (Str s2))) ce) ->
        Binop "++" (c1 . str $ s1 ++ s2) ce
    ("++", Binop "++" e (L _ _ (Literal (Str s1))), Literal (Str s2)) ->
        Binop "++" e (c1 . str $ s1 ++ s2)

    ("++", Data "[]" [], _) -> e2
    ("++", _, Data "[]" []) -> e1
    ("++", Data "::" [h,t], _) -> Data "::" [h, none $ binop "++" t ce2]

    ("|>", _, _) -> App ce2 ce1
    ("<|", _, _) -> App ce1 ce2
    (".", _, _) ->
        Lambda "x" (none $
                      App ce1 (none $ App ce2 (none $ Var "x")))

    _ | isAlpha (head op) || '_' == head op ->
          App (none $ App (none $ Var op) ce1) ce2
      | otherwise -> Binop op ce1 ce2