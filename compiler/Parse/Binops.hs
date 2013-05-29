module Parse.Binops (binops) where

import Ast
import Control.Monad (liftM,guard)
import Control.Monad.Error
import Data.List (foldl',splitAt,elemIndices
                 ,group,groupBy,sortBy,find,intercalate)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Located (epos)
import Text.Parsec
import Parse.Library

data Assoc = L | N | R deriving (Eq,Show)

type OpTable = [(Int, Assoc, String)]

table :: OpTable
table = [ (9, R, ".")
        , (8, R, "^")
        , (7, L, "*"), (7, L, "/"), (7, L, "mod"), (7, L, "div"), (7, L, "rem")
        , (6, L, "+"), (6, L, "-")
        , (5, R, "::"), (5, R, "++")
        , (4, N, "<="), (4, N, ">="), (4, N, "<")
        , (4, N, "=="), (4, N, "/="), (4, N, ">")
        , (4, L, "~"), (4, L, "<~")
        , (3, R, "&&")
        , (2, R, "||")
        , (0, R, "$")
        , (0, R, "<|"), (0, L, "|>")
        ]

opLevel op = Map.findWithDefault 9 op dict
    where dict = Map.fromList (map (\(lvl,_,op) -> (op,lvl)) table)

opAssoc op = Map.findWithDefault R op dict
    where dict = Map.fromList (map (\(_,assoc,op) -> (op,assoc)) table)

hasLevel n (op,e) = opLevel op == n

sortOps :: OpTable -> OpTable
sortOps = sortBy (\(i,_,_) (j,_,_) -> compare i j)

binops :: IParser CExpr -> IParser String -> IParser CExpr
binops term anyOp = do
  e <- term
  split 0 e =<< many (commitIf (whitespace >> anyOp) $ do
                        whitespace ; op <- anyOp
                        whitespace ; e <- term
                        return (op,e))

split :: Int -> CExpr -> [(String, CExpr)] -> IParser CExpr
split _ e []  = return e
split n e eops = do
  assoc <- getAssoc n eops
  es <- sequence (splitLevel n e eops)
  let ops = map fst (filter (hasLevel n) eops)
  case assoc of R -> joinR es ops
                _ -> joinL es ops

splitLevel :: Int -> CExpr -> [(String, CExpr)] -> [IParser CExpr]
splitLevel n e eops =
    case break (hasLevel n) eops of
      (lops, (op,e'):rops) -> split (n+1) e lops : splitLevel n e' rops
      (lops, []) -> [ split (n+1) e lops ]

joinL :: [CExpr] -> [String] -> IParser CExpr
joinL [e] [] = return e
joinL (a:b:es) (op:ops) = joinL (epos a b (Binop op a b) : es) ops
joinL _ _ = fail "Ill-formed binary expression. Report a compiler bug."

joinR :: [CExpr] -> [String] -> IParser CExpr
joinR [e] [] = return e
joinR (a:b:es) (op:ops) = do e <- joinR (b:es) ops
                             return (epos a e (Binop op a e))
joinR _ _ = fail "Ill-formed binary expression. Report a compiler bug."

getAssoc :: Int -> [(String,CExpr)] -> IParser Assoc
getAssoc n eops | all (==L) assocs = return L
                | all (==R) assocs = return R 
                | all (==N) assocs = case assocs of [_] -> return N
                                                    _   -> fail msg
    where levelOps = filter (hasLevel n) eops
          assocs = map (opAssoc . fst) levelOps
          msg = concat [ "Conflicting precedence for binary operators ("
                       , intercalate ", " (map fst eops), "). "
                       , "Consider adding parentheses to disambiguate." ]

