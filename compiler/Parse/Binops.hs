module Parse.Binops (binops, infixStmt, OpTable) where

import Control.Monad.Error
import Data.List (intercalate)
import qualified Data.Map as Map

import SourceSyntax.Location (merge)
import SourceSyntax.Expression (LExpr, Expr(Binop))
import SourceSyntax.Declaration (Assoc(..))
import Text.Parsec
import Parse.Helpers

type OpTable = [(Int, Assoc, String)]

preludeTable :: OpTable
preludeTable =
  [ (9, R, ".")
  , (8, R, "^")
  , (7, L, "*"), (7, L, "/"), (7, L, "mod"), (7, L, "div"), (7, L, "rem")
  , (6, L, "+"), (6, L, "-")
  , (5, R, "::"), (5, R, "++")
  , (4, N, "<="), (4, N, ">="), (4, N, "<")
  , (4, N, "=="), (4, N, "/="), (4, N, ">")
  , (4, L, "~"), (4, L, "<~")
  , (3, R, "&&")
  , (2, R, "||")
  , (0, R, "<|"), (0, L, "|>")
  ]

opLevel :: OpTable -> String -> Int
opLevel table op = Map.findWithDefault 9 op dict
    where dict = Map.fromList (map (\(lvl,_,op) -> (op,lvl)) table)

opAssoc :: OpTable -> String -> Assoc
opAssoc table op = Map.findWithDefault R op dict
    where dict = Map.fromList (map (\(_,assoc,op) -> (op,assoc)) table)

hasLevel :: OpTable -> Int -> (String,LExpr) -> Bool
hasLevel table n (op,_) = opLevel table op == n

binops :: OpTable -> IParser LExpr -> IParser String -> IParser LExpr
binops table term anyOp =
    do e <- term
       split (table ++ preludeTable) 0 e =<< many nextOp
    where
      nextOp = commitIf (whitespace >> anyOp) $ do
                 whitespace ; op <- anyOp
                 whitespace ; e <- term
                 return (op,e)

split :: OpTable -> Int -> LExpr -> [(String, LExpr)] -> IParser LExpr
split _ _ e []  = return e
split table n e eops = do
  assoc <- getAssoc table n eops
  es <- sequence (splitLevel table n e eops)
  let ops = map fst (filter (hasLevel table n) eops)
  case assoc of R -> joinR es ops
                _ -> joinL es ops

splitLevel :: OpTable -> Int -> LExpr -> [(String, LExpr)] -> [IParser LExpr]
splitLevel table n e eops =
    case break (hasLevel table n) eops of
      (lops, (op,e'):rops) ->
          split table (n+1) e lops : splitLevel table n e' rops
      (lops, []) -> [ split table (n+1) e lops ]

joinL :: [LExpr] -> [String] -> IParser LExpr
joinL [e] [] = return e
joinL (a:b:es) (op:ops) = joinL (merge a b (Binop op a b) : es) ops
joinL _ _ = fail "Ill-formed binary expression. Report a compiler bug."

joinR :: [LExpr] -> [String] -> IParser LExpr
joinR [e] [] = return e
joinR (a:b:es) (op:ops) = do e <- joinR (b:es) ops
                             return (merge a e (Binop op a e))
joinR _ _ = fail "Ill-formed binary expression. Report a compiler bug."

getAssoc :: OpTable -> Int -> [(String,LExpr)] -> IParser Assoc
getAssoc table n eops
    | all (==L) assocs = return L
    | all (==R) assocs = return R 
    | all (==N) assocs = case assocs of [_] -> return N
                                        _   -> fail msg
  where levelOps = filter (hasLevel table n) eops
        assocs = map (opAssoc table . fst) levelOps
        msg = concat [ "Conflicting precedence for binary operators ("
                     , intercalate ", " (map fst eops), "). "
                     , "Consider adding parentheses to disambiguate." ]

infixStmt :: IParser (Int, Assoc, String)
infixStmt =
  let infx str assoc = try (reserved ("infix" ++ str) >> return assoc) in
  do assoc <- choice [ infx "l" L, infx "r" R, infx "" N ]
     whitespace
     prec <- do n <- digit ; return (read [n] :: Int)
     whitespace
     op <- anyOp
     return (prec, assoc, op)