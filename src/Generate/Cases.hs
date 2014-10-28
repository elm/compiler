module Generate.Cases (toMatch, Match (..), Clause (..), matchSubst, newVar) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Data.List (groupBy,sortBy)
import Data.Maybe (fromMaybe)

import qualified AST.Annotation as A
import qualified AST.Expression.General as Expr
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Literal as Literal
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import Transform.Substitute (subst)

toMatch :: [(P.CanonicalPattern, Canonical.Expr)] -> State Int (String, Match)
toMatch patterns = do
  v <- newVar
  (,) v <$> match [v] (map (first (:[])) patterns) Fail


newVar :: State Int String
newVar =
  do  n <- State.get
      State.modify (+1)
      return $ "_v" ++ show n


data Match
    = Match String [Clause] Match
    | Break
    | Fail
    | Other Canonical.Expr
    | Seq [Match]
      deriving Show


data Clause =
    Clause (Either Var.Canonical Literal.Literal) [String] Match
    deriving Show


type Case =
    ([P.CanonicalPattern], Canonical.Expr)


matchSubst :: [(String,String)] -> Match -> Match
matchSubst pairs match =
    case match of
      Break -> Break

      Fail -> Fail
      
      Seq ms ->
          Seq (map (matchSubst pairs) ms)
      
      Other (A.A a e) ->
          Other . A.A a $ foldr ($) e $ map (\(x,y) -> subst x (Expr.localVar y)) pairs
      
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


match :: [String] -> [Case] -> Match -> State Int Match
match variables cases match =
    case (variables, cases, match) of
      ([], [], _) ->
          return match

      ([], [([], expr)], Fail) ->
          return $ Other expr

      ([], [([], expr)], Break) ->
          return $ Other expr

      ([], _, _) ->
          return $ Seq (map (Other . snd) cases ++ [match])

      (var:_, _, _)
          | all isVar cases' -> matchVar variables cases' match
          | all isCon cases' -> matchCon variables cases' match
          | otherwise        -> matchMix variables cases' match
          where
            cases' = map (dealias var) cases


dealias :: String -> Case -> Case
dealias _ ([], _) = noMatch "dealias"
dealias v c@(p:ps, A.A a e) =
    case p of
      P.Alias x pattern -> (pattern:ps, A.A a $ subst x (Expr.localVar v) e)
      _ -> c


matchVar :: [String] -> [Case] -> Match -> State Int Match
matchVar [] _ _ = noMatch "matchVar"
matchVar (v:vs) cs def =
    match vs (map subVar cs) def
  where
    subVar ([], _) = noMatch "matchVar.subVar"
    subVar (p:ps, (A.A a expr)) =
        (ps, A.A a $ subOnePattern p expr)
      where
        subOnePattern pattern expr =
            case pattern of
              P.Anything -> expr

              P.Var x ->
                  subst x (Expr.localVar v) expr

              P.Record fs ->
                 foldr (\x -> subst x (Expr.Access (A.A a (Expr.localVar v)) x)) expr fs

              _ -> noMatch "matchVar.subVar"


matchCon :: [String] -> [Case] -> Match -> State Int Match
matchCon variables cases match =
    case variables of
      [] -> noMatch "matchCon"

      var : _vars ->
          flip (Match var) match <$> mapM toClause caseGroups
        where
          caseGroups :: [[Case]]
          caseGroups =
              groupBy eq (sortBy cmp cases)

          eq :: Case -> Case -> Bool
          eq (patterns1, _) (patterns2, _) =
              case (patterns1, patterns2) of
                (pattern1:_, pattern2:_) ->
                    case (pattern1, pattern2) of
                      (P.Data name1 _, P.Data name2 _) -> name1 == name2
                      (_, _) -> pattern1 == pattern2
                (_, _) -> noMatch "matchCon.eq"

          cmp :: Case -> Case -> Ordering
          cmp (patterns1, _) (patterns2, _) =
              case (patterns1, patterns2) of
                (pattern1:_, pattern2:_) ->
                    case (pattern1, pattern2) of
                      (P.Data name1 _, P.Data name2 _) -> compare name1 name2
                      (_, _) -> compare pattern1 pattern2
                (_, _) -> noMatch "matchCon.cmp"

          toClause :: [Case] -> State Int Clause
          toClause cases =
              case head cases of
                (P.Data name _ : _, _) ->
                    matchClause (Left name) variables cases Break

                (P.Literal lit : _, _) ->
                    matchClause (Right lit) variables cases Break

                _ -> noMatch "matchCon"


matchClause :: Either Var.Canonical Literal.Literal -> [String] -> [Case] -> Match
            -> State Int Clause
matchClause _ [] _ _ = noMatch "matchClause"
matchClause c (_:vs) cs mtch =
    do vs' <- getVars
       Clause c vs' <$> match (vs' ++ vs) (map flatten cs) mtch
    where
      flatten ([], _) = noMatch "matchClause.flatten"
      flatten (p:ps, e) =
          case p of
            P.Data _ ps' -> (ps' ++ ps, e)
            P.Literal _  -> (ps, e)
            _ -> noMatch "matchClause.flatten"

      getVars :: State Int [String]
      getVars =
          case head cs of
            (P.Data _ ps : _, _) -> State.forM ps (const newVar)
            (P.Literal _ : _, _) -> return []
            _ -> noMatch "matchClause.getVars"


matchMix :: [String] -> [Case] -> Match -> State Int Match
matchMix vs cs def =
    State.foldM (flip $ match vs) def (reverse css)
  where
    css = groupBy (\p1 p2 -> isCon p1 == isCon p2) cs


noMatch :: String -> a
noMatch name =
    error $ "unexpected pattern in '" ++ name ++
            "' function. Report a compiler issue to <https://github.com/elm-lang/Elm>."
