
module Cases (caseToMatch, Match (..), Clause (..), matchSubst) where

import Control.Arrow (first)
import Control.Monad (liftM,foldM)
import Data.List (groupBy,sortBy,lookup)
import Data.Maybe (fromMaybe)

import Ast
import Context
import Guid
import Substitute

caseToMatch patterns = do
  v <- newVar
  match [v] (map (first (:[])) patterns) Fail

newVar = do n <- guid
            return $ "case" ++ show n

data Match = Match String [Clause] Match
           | Break
           | Fail
           | Other CExpr
           | Seq [Match]
             deriving Show

data Clause = Clause String [String] Match
              deriving Show

matchSubst :: [(String,String)] -> Match -> Match
matchSubst _ Break = Break
matchSubst _ Fail = Fail
matchSubst pairs (Seq ms) = Seq (map (matchSubst pairs) ms)
matchSubst pairs (Other (C t s e)) =
    Other . C t s $ foldr ($) e $ map (\(x,y) -> subst x (Var y)) pairs
matchSubst pairs (Match n cs m) =
    Match (varSubst n) (map clauseSubst cs) (matchSubst pairs m)
        where varSubst v = fromMaybe v (lookup v pairs)
              clauseSubst (Clause c vs m) =
                  Clause c (map varSubst vs) (matchSubst pairs m)

isCon (PData _ _ : _, _) = True
isCon _                  = False

isVar p = not (isCon p)

match :: [String] -> [([Pattern],CExpr)] -> Match -> GuidCounter Match
match [] [] def = return def
match [] [([],e)] Fail  = return $ Other e
match [] [([],e)] Break = return $ Other e
match [] cs def = return $ Seq (map (Other . snd) cs ++ [def])
match vs cs def
    | all isVar cs = matchVar vs cs def
    | all isCon cs = matchCon vs cs def
    | otherwise    = matchMix vs cs def

matchVar :: [String] -> [([Pattern],CExpr)] -> Match -> GuidCounter Match
matchVar (v:vs) cs def = match vs (map subVar cs) def
    where subVar (PVar x    : ps, C t s e) = (ps, C t s $ subst x (Var v) e)
          subVar (PAnything : ps, ce) = (ps, ce)

matchCon :: [String] -> [([Pattern],CExpr)] -> Match -> GuidCounter Match
matchCon (v:vs) cs def = (flip (Match v) def) `liftM` mapM toClause css
    where css = groupBy (withName (==)) $ sortBy (withName compare) cs
          withName f (PData n1 _:_,_) (PData n2 _:_,_) = f n1 n2
          toClause cs = let (PData name _ : _ , _) = head cs in
                        matchClause name (v:vs) cs Break

matchClause :: String -> [String] -> [([Pattern],CExpr)] -> Match -> GuidCounter Clause
matchClause c (v:vs) cs def =
    do vs' <- getVars
       Clause c vs' `liftM` match (vs' ++ vs) (map flatten cs) def
    where flatten (PData _ ps' : ps, e) = (ps' ++ ps, e)
          getVars = let (PData _ ps : _, _) = head cs in
                    mapM (\_ -> newVar) ps

matchMix :: [String] -> [([Pattern],CExpr)] -> Match -> GuidCounter Match
matchMix vs cs def = foldM (flip $ match vs) def (reverse css)
    where css = groupBy (\p1 p2 -> isCon p1 == isCon p2) cs