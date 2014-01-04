module Generate.Cases (toMatch, Match (..), Clause (..), matchSubst, newVar) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad.State
import Data.List (groupBy,sortBy)
import Data.Maybe (fromMaybe)

import SourceSyntax.Location
import SourceSyntax.Literal
import SourceSyntax.Pattern
import SourceSyntax.Expression
import Transform.Substitute


toMatch :: [(Pattern, LExpr)] -> State Int (String, Match)
toMatch patterns = do
  v <- newVar
  (,) v <$> match [v] (map (first (:[])) patterns) Fail

newVar :: State Int String
newVar = do n <- get
            modify (+1)
            return $ "_v" ++ show n

data Match
    = Match String [Clause] Match
    | Break
    | Fail
    | Other LExpr
    | Seq [Match]
      deriving Show

data Clause =
    Clause (Either String Literal) [String] Match
    deriving Show

matchSubst :: [(String,String)] -> Match -> Match
matchSubst _ Break = Break
matchSubst _ Fail = Fail
matchSubst pairs (Seq ms) = Seq (map (matchSubst pairs) ms)
matchSubst pairs (Other (L s e)) =
    Other . L s $ foldr ($) e $ map (\(x,y) -> subst x (Var y)) pairs
matchSubst pairs (Match n cs m) =
    Match (varSubst n) (map clauseSubst cs) (matchSubst pairs m)
        where varSubst v = fromMaybe v (lookup v pairs)
              clauseSubst (Clause c vs m) =
                  Clause c (map varSubst vs) (matchSubst pairs m)

isCon (p:_, _) =
  case p of
    PData _ _  -> True
    PLiteral _ -> True
    _          -> False

isVar p = not (isCon p)

match :: [String] -> [([Pattern],LExpr)] -> Match -> State Int Match
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

dealias v c@(p:ps, L s e) =
    case p of
      PAlias x pattern -> (pattern:ps, L s $ subst x (Var v) e)
      _ -> c

matchVar :: [String] -> [([Pattern],LExpr)] -> Match -> State Int Match
matchVar (v:vs) cs def = match vs (map subVar cs) def
  where
    subVar (p:ps, (L s e)) = (ps, L s $ subOnePattern p e)
        where
          subOnePattern pattern e =
            case pattern of
              PVar x     -> subst x (Var v) e
              PAnything  -> e
              PRecord fs ->
                 foldr (\x -> subst x (Access (L s (Var v)) x)) e fs

matchCon :: [String] -> [([Pattern],LExpr)] -> Match -> State Int Match
matchCon (v:vs) cs def = (flip (Match v) def) <$> mapM toClause css
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

matchClause :: Either String Literal
            -> [String]
            -> [([Pattern],LExpr)]
            -> Match
            -> State Int Clause
matchClause c (_:vs) cs def =
    do vs' <- getVars
       Clause c vs' <$> match (vs' ++ vs) (map flatten cs) def
    where

      flatten (p:ps, e) =
          case p of
            PData _ ps' -> (ps' ++ ps, e)
            PLiteral _  -> (ps, e)

      getVars =
          case head cs of
            (PData _ ps : _, _) -> forM ps (const newVar)
            (PLiteral _ : _, _) -> return []

matchMix :: [String] -> [([Pattern],LExpr)] -> Match -> State Int Match
matchMix vs cs def = foldM (flip $ match vs) def (reverse css)
    where css = groupBy (\p1 p2 -> isCon p1 == isCon p2) cs