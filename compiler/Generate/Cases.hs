{-# OPTIONS_GHC -W #-}
module Generate.Cases (toMatch, Match (..), Clause (..), matchSubst, newVar) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad.State
import Data.List (groupBy,sortBy)
import Data.Maybe (fromMaybe)

import AST.Annotation 
import AST.Expression.General
import AST.Expression.Canonical as Canonical
import AST.Literal
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import Transform.Substitute

toMatch :: [(P.CanonicalPattern, Canonical.Expr)] -> State Int (String, Match)
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
    | Other Canonical.Expr
    | Seq [Match]
      deriving Show

data Clause =
    Clause (Either Var.Canonical Literal) [String] Match
    deriving Show

matchSubst :: [(String,String)] -> Match -> Match
matchSubst pairs match =
    case match of
      Break -> Break
      Fail  -> Fail
      Seq ms -> Seq (map (matchSubst pairs) ms)
      Other (A a e) ->
          Other . A a $ foldr ($) e $ map (\(x,y) -> subst x (localVar y)) pairs
      Match n cs m ->
          Match (varSubst n) (map clauseSubst cs) (matchSubst pairs m)
          where
            varSubst v = fromMaybe v (lookup v pairs)
            clauseSubst (Clause c vs m) =
                Clause c (map varSubst vs) (matchSubst pairs m)

isCon :: ([P.Pattern var], expr) -> Bool
isCon ([] , _) = noMatch "isCon"
isCon (p:_, _) =
  case p of
    P.Data _ _  -> True
    P.Literal _ -> True
    _           -> False

isVar :: ([P.Pattern var], expr) -> Bool
isVar p = not (isCon p)

match :: [String] -> [([P.CanonicalPattern], Canonical.Expr)] -> Match -> State Int Match
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

dealias :: String -> ([P.CanonicalPattern], Canonical.Expr) -> ([P.CanonicalPattern], Canonical.Expr)
dealias _ ([], _) = noMatch "dealias"
dealias v c@(p:ps, A a e) =
    case p of
      P.Alias x pattern -> (pattern:ps, A a $ subst x (localVar v) e)
      _ -> c

matchVar :: [String] -> [([P.CanonicalPattern], Canonical.Expr)] -> Match -> State Int Match
matchVar [] _ _ = noMatch "matchVar"
matchVar (v:vs) cs def = match vs (map subVar cs) def
  where
    subVar ([], _) = noMatch "matchVar.subVar"
    subVar (p:ps, (A a e)) = (ps, A a $ subOnePattern p e)
        where
          subOnePattern pattern e =
            case pattern of
              P.Var x     -> subst x (localVar v) e
              P.Anything  -> e
              P.Record fs ->
                 foldr (\x -> subst x (Access (A a (localVar v)) x)) e fs
              _ -> noMatch "matchVar.subVar"

matchCon :: [String] -> [([P.CanonicalPattern], Canonical.Expr)] -> Match -> State Int Match
matchCon [] _ _ = noMatch "matchCon"
matchCon (v:vs) cs def = (flip (Match v) def) <$> mapM toClause css
    where
      css = groupBy eq (sortBy cmp cs)

      cmp (p1:_,_) (p2:_,_) =
        case (p1,p2) of
          (P.Data n1 _, P.Data n2 _) -> compare n1 n2
          _ -> compare p1 p2
      cmp _ _ = noMatch "matchCon.cmp"

      eq (p1:_,_) (p2:_,_) =
        case (p1,p2) of
          (P.Data n1 _, P.Data n2 _) -> n1 == n2
          _ -> p1 == p2
      eq _ _ = noMatch "matchCon.eq"

      toClause cs =
        case head cs of
          (P.Data name _ : _, _) -> matchClause (Left name) (v:vs) cs Break
          (P.Literal lit : _, _) -> matchClause (Right lit) (v:vs) cs Break
          _ -> noMatch "matchCon"

matchClause :: Either Var.Canonical Literal
            -> [String]
            -> [([P.CanonicalPattern], Canonical.Expr)]
            -> Match
            -> State Int Clause
matchClause _ [] _ _ = noMatch "matchClause"
matchClause c (_:vs) cs def =
    do vs' <- getVars
       Clause c vs' <$> match (vs' ++ vs) (map flatten cs) def
    where
      flatten ([], _) = noMatch "matchClause.flatten"
      flatten (p:ps, e) =
          case p of
            P.Data _ ps' -> (ps' ++ ps, e)
            P.Literal _  -> (ps, e)
            _ -> noMatch "matchClause.flatten"

      getVars =
          case head cs of
            (P.Data _ ps : _, _) -> forM ps (const newVar)
            (P.Literal _ : _, _) -> return []
            _ -> noMatch "matchClause.getVars"

matchMix :: [String] -> [([P.CanonicalPattern], Canonical.Expr)] -> Match -> State Int Match
matchMix vs cs def = foldM (flip $ match vs) def (reverse css)
    where css = groupBy (\p1 p2 -> isCon p1 == isCon p2) cs

noMatch :: String -> a
noMatch name =
    error $ "unexpected pattern in '" ++ name ++
            "' function. Report a compiler issue to <https://github.com/elm-lang/Elm>."
