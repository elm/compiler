module Generate.Cases (toMatch, Match (..), Clause (..), matchSubst, newVar) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import qualified Data.List as List
import Data.Maybe (fromMaybe)

import qualified AST.Expression.General as Expr
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Literal as Literal
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import qualified Generate.Substitute as S
import qualified Reporting.Annotation as A


-- MATCHES

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


type Branch =
    ([P.CanonicalPattern], Canonical.Expr)


-- PUBLIC FUNCTIONS

toMatch :: [(P.CanonicalPattern, Canonical.Expr)] -> State Int (String, Match)
toMatch patterns =
  do  v <- newVar
      (,) v <$> buildMatch [v] (map (first (:[])) patterns) Fail


newVar :: State Int String
newVar =
  do  n <- State.get
      State.modify (+1)
      return $ "_v" ++ show n


matchSubst :: [(String,String)] -> Match -> Match
matchSubst pairs match =
  case match of
    Break -> Break

    Fail -> Fail

    Seq ms ->
        Seq (map (matchSubst pairs) ms)

    Other (A.A a e) ->
        Other . A.A a $ foldr ($) e $ map (\(x,y) -> S.subst x (Expr.localVar y)) pairs

    Match n cs m ->
        Match (varSubst n) (map clauseSubst cs) (matchSubst pairs m)
      where
        varSubst v = fromMaybe v (lookup v pairs)
        clauseSubst (Clause c vs m) =
            Clause c (map varSubst vs) (matchSubst pairs m)


-- BUILD MATCHES

buildMatch :: [String] -> [Branch] -> Match -> State Int Match
buildMatch variables branches match =
    case (variables, branches, match) of
      ([], [], _) ->
          return match

      ([], [([], expr)], Fail) ->
          return $ Other expr

      ([], [([], expr)], Break) ->
          return $ Other expr

      ([], _, _) ->
          return $ Seq (map (Other . snd) branches ++ [match])

      (var:_, _, _)
          | all isVar branches' -> matchVar variables branches' match
          | all isCon branches' -> matchCon variables branches' match
          | otherwise           -> matchMix variables branches' match
          where
            branches' = map (dealias var) branches


dealias :: String -> Branch -> Branch
dealias _ ([], _) = noMatch "dealias"
dealias var branch@(p:ps, A.A ann e) =
    case p of
      A.A _ (P.Alias alias pattern) ->
          ( pattern:ps
          , A.A ann (S.subst alias (Expr.localVar var) e)
          )

      _ ->
          branch


isCon :: ([P.Pattern ann var], expr) -> Bool
isCon ([], _) = noMatch "isCon"
isCon (A.A _ pattern : _, _) =
    case pattern of
      P.Data _ _ -> True
      P.Literal _ -> True
      _ -> False


isVar :: ([P.Pattern ann var], expr) -> Bool
isVar p =
    not (isCon p)


-- VARIABLE MATCHES

matchVar :: [String] -> [Branch] -> Match -> State Int Match
matchVar [] _ _ = noMatch "matchVar"
matchVar (var:vars) branches match =
    buildMatch vars (map subVar branches) match
  where
    subVar ([], _) = noMatch "matchVar.subVar"
    subVar (A.A _ pattern : patterns, (A.A ann expr)) =
        ( patterns
        , A.A ann (subOnePattern pattern expr)
        )
      where
        subOnePattern pattern expr =
            case pattern of
              P.Anything ->
                  expr

              P.Var x ->
                  S.subst x (Expr.localVar var) expr

              P.Record fields ->
                  let access = Expr.Access (A.A ann (Expr.localVar var))
                  in
                      foldr (\x -> S.subst x (access x)) expr fields

              _ ->
                  noMatch "matchVar.subVar"


-- CONSTRUCTOR MATCHES

matchCon :: [String] -> [Branch] -> Match -> State Int Match
matchCon variables branches match =
    case variables of
      [] -> noMatch "matchCon"

      var : _vars ->
          flip (Match var) match <$> mapM toClause caseGroups
        where
          caseGroups :: [[Branch]]
          caseGroups =
              List.groupBy branchEq (List.sortBy branchCmp branches)

          toClause :: [Branch] -> State Int Clause
          toClause branches =
              case head branches of
                (A.A _ (P.Data name _) : _, _) ->
                    matchClause (Left name) variables branches Break

                (A.A _ (P.Literal lit) : _, _) ->
                    matchClause (Right lit) variables branches Break

                _ -> noMatch "matchCon"


matchClause
    :: Either Var.Canonical Literal.Literal
    -> [String]
    -> [Branch]
    -> Match
    -> State Int Clause
matchClause _ [] _ _ = noMatch "matchClause"
matchClause c (_:vs) branches match =
  do  vs' <- getVars
      Clause c vs' <$> buildMatch (vs' ++ vs) (map flatten branches) match
  where
    flatten ([], _) = noMatch "matchClause.flatten"
    flatten (A.A _ pattern : patterns, expr) =
        case pattern of
          P.Data _ argPatterns ->
              (argPatterns ++ patterns, expr)

          P.Literal _  ->
              (patterns, expr)

          _ ->
              noMatch "matchClause.flatten"

    getVars :: State Int [String]
    getVars =
        case head branches of
          (A.A _ (P.Data _ argPatterns) : _, _) ->
              State.forM argPatterns (const newVar)

          (A.A _ (P.Literal _) : _, _) ->
              return []

          _ ->
              noMatch "matchClause.getVars"


branchEq :: Branch -> Branch -> Bool
branchEq (A.A _ pattern : _, _) (A.A _ pattern' : _, _) =
  case (pattern, pattern') of
    (P.Data name _, P.Data name' _) ->
        name == name'

    (P.Literal lit, P.Literal lit') ->
        lit == lit'

    (_, _) ->
        noMatch "matchCon.branchEq"

branchEq _ _ = noMatch "matchCon.branchEq"


branchCmp :: Branch -> Branch -> Ordering
branchCmp (A.A _ pattern : _, _) (A.A _ pattern' : _, _) =
  case (pattern, pattern') of
    (P.Data name _, P.Data name' _) ->
        compare name name'

    (P.Literal lit, P.Literal lit') ->
        compare lit lit'

    (_, _) ->
        noMatch "matchCon.branchCmp"

branchCmp _ _ = noMatch "matchCon.branchCmp"


-- MIX MATCHES

matchMix :: [String] -> [Branch] -> Match -> State Int Match
matchMix vs branches def =
    State.foldM (flip $ buildMatch vs) def (reverse branchGroups)
  where
    branchGroups =
        List.groupBy (\p1 p2 -> isCon p1 == isCon p2) branches


noMatch :: String -> a
noMatch name =
    error $ "unexpected pattern in `" ++ name ++
            "` function. Report a compiler issue to <https://github.com/elm-lang/Elm>."
