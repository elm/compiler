module Generate.Cases (caseToMatch, Match (..), Clause (..), matchSubst) where

import Control.Arrow (first)
import Control.Monad (liftM,foldM)
import Data.List (groupBy,sortBy,lookup)
import Data.Maybe (fromMaybe)
import Data.Data

import Unique
import SourceSyntax.Location
import SourceSyntax.Literal
import SourceSyntax.Pattern
import SourceSyntax.Expression
import Transform.Substitute

caseToMatch patterns = do
  v <- newVar
  match [v] (map (first (:[])) patterns) Fail

newVar = do n <- guid
            return $ "case" ++ show n

data Match t v
    = Match String [Clause t v] (Match t v)
    | Break
    | Fail
    | Other (LExpr t v)
    | Seq [Match t v]
      deriving Show

data Clause t v =
    Clause (Either String Literal) [String] (Match t v)
    deriving Show

matchSubst :: (Data t, Data v) => [(String,String)] -> Match t v -> Match t v
matchSubst _ Break = Break
matchSubst _ Fail = Fail
matchSubst pairs (Seq ms) = Seq (map (matchSubst pairs) ms)
matchSubst pairs (Other (L t s e)) =
    Other . L t s $ foldr ($) e $ map (\(x,y) -> subst x (Var y)) pairs
matchSubst pairs (Match n cs m) =
    Match (varSubst n) (map clauseSubst cs) (matchSubst pairs m)
        where varSubst v = fromMaybe v (lookup v pairs)
              clauseSubst (Clause c vs m) =
                  Clause c (map varSubst vs) (matchSubst pairs m)

isCon (p:ps, e) =
  case p of
    PData _ _  -> True
    PLiteral _ -> True
    _          -> False

isVar p = not (isCon p)

match :: (Data t, Data v) => [String] -> [([Pattern],LExpr t v)] -> Match t v -> Unique (Match t v)
match [] [] def = return def
match [] [([],e)] Fail  = return $ Other e
match [] [([],e)] Break = return $ Other e
match [] cs def = return $ Seq (map (Other . snd) cs ++ [def])
match vs@(v:_) cs def
    | all isVar cs' = matchVar vs cs' def
    | all isCon cs' = matchCon vs cs' def
    | otherwise     = matchMix vs cs' def
  where
    cs' = map (dealias v) cs

dealias v c@(p:ps, L t s e) =
    case p of
      PAlias x pattern -> (pattern:ps, L t s $ subst x (Var v) e)
      _ -> c

matchVar :: (Data t, Data v) => [String] -> [([Pattern],LExpr t v)] -> Match t v
         -> Unique (Match t v)
matchVar (v:vs) cs def = match vs (map subVar cs) def
  where
    subVar (p:ps, ce@(L t s e)) = (ps, L t s $ subOnePattern p e)
        where
          loc = L t s
          subOnePattern pattern e =
            case pattern of
              PVar x     -> subst x (Var v) e
              PAnything  -> e
              PRecord fs ->
                 foldr (\x -> subst x (Access (loc (Var v)) x)) e fs

matchCon :: (Data t, Data v) => [String] -> [([Pattern],LExpr t v)] -> Match t v
         -> Unique (Match t v)
matchCon (v:vs) cs def = (flip (Match v) def) `liftM` mapM toClause css
    where
      css = groupBy eq (sortBy cmp cs)

      cmp (p1:_,_) (p2:_,_) =
        case (p1,p2) of
          (PData n1 _, PData n2 _) -> compare n1 n2
          _ -> compare p1 p2

      eq (p1:_,_) (p2:_,_) =
        case (p1,p2) of
          (PData n1 _, PData n2 _) -> n1 == n2
          _ -> p1 == p2

      toClause cs =
        case head cs of
          (PData name _ : _, _) -> matchClause (Left name) (v:vs) cs Break
          (PLiteral lit : _, _) -> matchClause (Right lit) (v:vs) cs Break

matchClause :: (Data t, Data v) => Either String Literal
            -> [String]
            -> [([Pattern],LExpr t v)]
            -> Match t v
            -> Unique (Clause t v)
matchClause c (v:vs) cs def =
    do vs' <- getVars
       Clause c vs' `liftM` match (vs' ++ vs) (map flatten cs) def
    where

      flatten (p:ps, e) =
          case p of
            PData name ps' -> (ps' ++ ps, e)
            PLiteral _  -> (ps, e)

      getVars =
          case head cs of
            (PData _ ps : _, _) -> mapM (\_ -> newVar) ps
            (PLiteral _ : _, _) -> return []

matchMix :: (Data t, Data v) => [String] -> [([Pattern],LExpr t v)] -> Match t v
         -> Unique (Match t v)
matchMix vs cs def = foldM (flip $ match vs) def (reverse css)
    where css = groupBy (\p1 p2 -> isCon p1 == isCon p2) cs