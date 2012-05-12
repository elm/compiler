module Initialize (initialize) where

import Ast
import Control.Arrow (first)
import Control.Monad
import Data.Char (isAlpha)
import Data.Maybe (mapMaybe)
import Data.Either (rights)
import FreeVar
import Lexer
import Parser (toExpr,toDefs)
import Rename (rename)
import Replace
import System.IO.Unsafe

import Unify

initialize str = unify =<< toDefs =<< tokenize str


initialize' str = do
   expr <- toDefs =<< tokenize str
--   let rexpr = rename expr
--   let init = simp_loop rexpr
--   return $ if depth init < depth rexpr then init else rexpr
   return $ rename expr
{--
simp_loop exp = if exp == exp' then exp' else simp_loop exp'
    where exp' = simp exp

simp expr =
    let f = simp in
    case expr of
      Range e1 e2 -> simp_range (f e1) (f e2)
      Binop op e1 e2 -> simp_binop op (f e1) (f e2)
      Lambda x e -> Lambda x (f e)
      App e1 e2 -> simp_app (f e1) (f e2)
      If e1 e2 e3 -> simp_if (f e1) (f e2) (f e3)
      Lift e es -> Lift (f e) $ map f es
      Fold e1 e2 e3 -> Fold (f e1) (f e2) (f e3)
      Async e -> Async (f e)
      Let x e1 e2 -> simp_let x (f e1) (f e2)
      Data name es -> Data name (map f es)
      Case e cases -> simp_case (f e) cases
      _ -> expr

data Status = NoMatch | Quit deriving (Show,Eq)

simp_case v cases =
    case v of
      Data _ _ -> case rights $ map (\(p,e) -> match p v e) cases of
                    { v:_ -> v; _ -> Case v cases }
      _ -> Case v cases

match p v hole =
    case p of PAnything -> return hole
              PVar x -> return $ Let x v hole
              PData pn ps ->
                  case v of
                    Data vn vs -> if pn /= vn then Left NoMatch else
                                      foldM (flip ($)) hole (zipWith match ps vs)
                    _ -> Left Quit

simp_let x e1 e2
    | not (x `freeIn` e2) = e2
    | isValue e1 = Let x e1 (replace x e1 e2)
    | otherwise  = Let x e1 e2

simp_app e1 e2 =
    case e1 of
      Let x s e -> if x `freeIn` e2 then error "Overlapping names!" else
                       Let x s (App e e2)
      Lambda x e -> Let x e2 e
{--
      Var "constant" -> App (Lambda "v" (Lift (Var "v") [])) e2
      Var "async" -> Async e2
      Var "foldp" -> Lambda "b" . Lambda "s" $ (Fold e2 (Var "b") (Var "s"))
      Var "lift"  -> Lambda "s" (Lift e2 [Var "s"])
      Var "lift2" -> Lambda "s1" . Lambda "s2" $ (Lift e2 [Var "s1",Var "s2"])
--}
      _ -> App e1 e2

simp_if (Boolean b) e2 e3 = if b then e2 else e3
simp_if a b c = If a b c

isValue expr =
    case expr of
      Number _ -> True
      Chr _ -> True
      Boolean _ -> True
      Range _ _ -> True
      Lambda _ _ -> True
      Data _ _ -> True
      Var _ -> True
      _ -> False

simp_range lo hi = toAST (parse code)
    where toAST = either (error "Compilation Failure") (\f -> App (App f lo) hi)
          parse str = toExpr =<< tokenize str
          code = "let range xs lo hi = \
                 \if hi < lo then xs else range (hi:xs) lo (hi-1) in\
                 \ \\x y -> range [] x y"

simp_binop "mod" (Number n) (Number m) = Number (mod n m)
simp_binop "mod" e1 e2 = Binop "mod" e1 e2
simp_binop str e1 e2
    | isAlpha (head str) || '_' == head str =
        App (App (Var str) e1) e2
    | otherwise = binop str e1 e2

binop op (Number n) (Number m) = f n m
    where f a b = case op of
                    { "+" -> Number $ (+) a b
                    ; "-" -> Number $ (-) a b
                    ; "*" -> Number $ (*) a b
                    --; "/" -> Number $ div a b
                    ; "<" -> Boolean $ a < b
                    ; ">" -> Boolean $ a < b
                    ; "<=" -> Boolean $ a <= b
                    ; ">=" -> Boolean $ a >= b
                    ; "==" -> Boolean $ a == b
                    ; "/=" -> Boolean $ a /= b
                    ;  _  -> Binop op (Number n) (Number m) }

binop "-" e (Number 0) = e
binop "+" (Number 0) e = e
binop "+" (Number n) (Binop "+" (Number m) e) = binop "+" (Number (n+m)) e
binop "+" (Number n) (Binop "+" e (Number m)) = binop "+" (Number (n+m)) e

binop "/" e (Number 1) = e
binop "*" (Number 0) e = Number 0
binop "*" (Number 1) e = e
binop "*" (Number n) (Binop "*" (Number m) e) = binop "*" (Number (n*m)) e
binop "*" (Number n) (Binop "*" e (Number m)) = binop "*" (Number (n*m)) e

binop "+" e (Number n) = binop "+" (Number n) e
binop "*" e (Number n) = binop "*" (Number n) e

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
binop "++" (Data "Nil" []) e = e
binop "++" e (Data "Nil" []) = e
binop "++" (Data "Cons" [h,t]) e = Data "Cons" [h, binop "++" t e]

binop "$" e1 e2 = App e1 e2

binop op e1 e2 = Binop op e1 e2
--}