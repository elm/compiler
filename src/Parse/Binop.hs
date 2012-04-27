module Binop (binops) where

import Ast
import Combinators
import Control.Monad (liftM,guard)
import Control.Monad.Error
import Data.List (foldl',splitAt,elemIndices,group,groupBy,sortBy,find)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

data Assoc = L | Non | R deriving (Eq,Show)

table = [ (9, R, ".")
        , (7, L, "*"), (7, L, "/"), (7, L, "mod")
        , (6, L, "+"), (6, L, "-")
        , (5, R, ":" ), (5, R, "++")
        , (4, Non, "<="), (4, Non, ">="), (4, Non, "<")
        , (4, Non, "=="), (4, Non, "/="), (4, Non, ">")
        , (3, R, "&&")
        , (2, R, "||")
        , (0, R, "$")
        ]

sortOps = sortBy (\(i,_,_) (j,_,_) -> compare i j)

binops term anyOp = do
  e <- term
  (ops,es) <- liftM unzip $ star (do { op <- anyOp; e <- term; return (op,e) })
  case binopOf (Map.empty) (sortOps table) ops (e:es) of
    Right e -> return e
    Left msg -> zero

binopSplit seen opTable i ops es =
    case (splitAt i ops, splitAt (i+1) es) of
      ((befores, op:afters), (pres, posts)) ->
          do e1 <- binopOf seen opTable befores pres
             e2 <- binopOf seen opTable afters posts
             return $ Binop op e1 e2

binopOf _ _ _ [e] = return e
binopOf _ [] ops es =
    return $ foldl (flip ($)) (head es) $ zipWith Binop ops (tail es)

binopOf seen (tbl@((lvl, L, op):rest)) ops es =
    case elemIndices op ops of
      [] -> binopOf seen rest ops es
      is -> case Map.lookup lvl seen of
              Nothing -> binopSplit (Map.insert lvl (L,op) seen) tbl (last is) ops es
              Just (L,_) -> binopSplit seen tbl (last is) ops es
              Just (assoc,op') -> Left $ errorMessage lvl op L op' assoc
binopOf seen (tbl@((lvl, assoc, op):rest)) ops es =
    case elemIndices op ops of
      [] -> binopOf seen rest ops es
      i:_ -> case Map.lookup lvl seen of
               Nothing -> binopSplit (Map.insert lvl (R,op) seen) tbl i ops es
               Just (assoc',op') ->
                   if assoc == assoc' && assoc /= Non then
                       binopSplit seen tbl i ops es
                   else Left $ errorMessage lvl op assoc op' assoc'

errorMessage lvl op1 assoc1 op2 assoc2 =
    "Cannot have (" ++ op1 ++ ") with [" ++ show assoc1 ++ " " ++
    show lvl ++ "] and (" ++ op2 ++ ") with [" ++ show assoc2 ++ " " ++
    show lvl ++ "]"
