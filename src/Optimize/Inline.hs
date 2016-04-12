{-# OPTIONS_GHC -Wall #-}
module Optimize.Inline (inline) where

import Control.Arrow (second, (***))
import Control.Monad (foldM)
import qualified Data.List as List
import qualified Data.Map as Map

import AST.Expression.Optimized (Expr(..), Decider(..), Choice(..))
import qualified AST.Expression.Optimized as Opt
import qualified AST.Variable as Var
import qualified Optimize.Environment as Env



inline :: Map.Map String Opt.Expr -> Opt.Expr -> Env.Optimizer Opt.Expr
inline rawSubs expression =
  let
    counts =
      count expression

    annotatedSubs =
      Map.toList (Map.intersectionWith (,) counts rawSubs)
  in
    do  (Subs subs defs) <- foldM processSubs (Subs Map.empty []) annotatedSubs
        let inlinedExpr = replace subs expression
        return $
          if null defs then
            inlinedExpr

          else
            Let (map (\(name, expr) -> Opt.Def Opt.dummyFacts name expr) defs) inlinedExpr


data Subs = Subs
    { _substitutions :: Map.Map String Opt.Expr
    , _definitions :: [(String, Opt.Expr)]
    }


processSubs :: Subs -> (String, (Int, Opt.Expr)) -> Env.Optimizer Subs
processSubs (Subs subs defs) (name, (n, expr)) =
  if n == 1 then
      return $ Subs (Map.insert name expr subs) defs

  else
      do  uniqueName <- Env.freshName
          return $ Subs
              (Map.insert name (Opt.Var (Var.local uniqueName)) subs)
              ((uniqueName, expr) : defs)



-- HELPERS


deleteBatch :: [String] -> Map.Map String a -> Map.Map String a
deleteBatch names dict =
  List.foldl' (flip Map.delete) dict names


getDefName :: Opt.Def -> String
getDefName def =
  case def of
    Opt.Def _ name _ ->
        name

    Opt.TailDef _ name _ _ ->
        name



-- COUNT VARIABLE APPEARANCES


count :: Opt.Expr -> Map.Map String Int
count expression =
  let
    count2 a b =
      Map.unionWith (+) (count a) (count b)

    countMany xs =
      Map.unionsWith (+) (map count xs)
  in
  case expression of
    Literal _ ->
        Map.empty

    Var (Var.Canonical home name) ->
        case home of
          Var.Local ->
              Map.singleton name 1

          Var.BuiltIn ->
              Map.empty

          Var.Module _ ->
              Map.empty

          Var.TopLevel _ ->
              Map.empty

    Range lo hi ->
        count2 lo hi

    ExplicitList exprs ->
        countMany exprs

    Binop _op left right ->
        count2 left right

    Function args body ->
        deleteBatch args (count body)

    Call func args ->
        Map.unionWith (+) (count func) (countMany args)

    TailCall _name _argNames args ->
        countMany args

    If branches finally ->
        Map.unionsWith (+) (count finally : map (uncurry count2) branches)

    Let defs expr ->
        let
          countDef def =
            case def of
              Opt.Def _ name body ->
                  Map.delete name (count body)

              Opt.TailDef _ name argNames body ->
                  deleteBatch (name:argNames) (count body)

          exprCount =
            deleteBatch (map getDefName defs) (count expr)
        in
          Map.unionsWith (+) (exprCount : map countDef defs)

    Case _ decider jumps ->
        let
          countDecider dcdr =
            case dcdr of
              Leaf (Inline expr) ->
                  count expr

              Leaf (Jump _)  ->
                  Map.empty

              Chain _ success failure ->
                  Map.unionWith (+) (countDecider success) (countDecider failure)

              FanOut _path tests fallback ->
                  Map.unionsWith (+) (map countDecider (fallback : map snd tests))
        in
          Map.unionsWith (+) (countDecider decider : map (count . snd) jumps)

    Data _tag values ->
        countMany values

    DataAccess root _index ->
        count root

    Access record _field ->
        count record

    Update record fields ->
        Map.unionWith (+) (count record) (countMany (map snd fields))

    Record fields ->
        countMany (map snd fields)

    Cmd _ ->
        Map.empty

    Sub _ ->
        Map.empty

    OutgoingPort _ _ ->
        Map.empty

    IncomingPort _ _ ->
        Map.empty

    Program _ expr ->
        count expr

    GLShader _ _ _ ->
        Map.empty

    Crash _ _ maybeBranchProblem ->
        maybe Map.empty count maybeBranchProblem



-- REPLACE


replace :: Map.Map String Opt.Expr -> Opt.Expr -> Opt.Expr
replace substitutions expression =
  let
    go = replace substitutions
  in
  case expression of
    Literal _ ->
        expression

    Var (Var.Canonical Var.Local name) ->
        maybe expression id (Map.lookup name substitutions)

    Var _ ->
        expression

    Range lo hi ->
        Range (go lo) (go hi)

    ExplicitList exprs ->
        ExplicitList (map go exprs)

    Binop op left right ->
        Binop op (go left) (go right)

    Function args body ->
        Function args (replace (deleteBatch args substitutions) body)

    Call func args ->
        Call (go func) (map go args)

    TailCall name argNames args ->
        TailCall name argNames (map go args)

    If branches finally ->
        If (map (go *** go) branches) (go finally)

    Let defs expr ->
        let
          replaceDef def =
            case def of
              Opt.Def facts name body ->
                  Opt.Def facts name
                      (replace (Map.delete name substitutions) body)

              Opt.TailDef facts name argNames body ->
                  Opt.TailDef facts name argNames
                      (replace (deleteBatch (name:argNames) substitutions) body)

          boundNames =
            map getDefName defs
        in
          Let
            (map replaceDef defs)
            (replace (deleteBatch boundNames substitutions) expr)

    Case exprName decider jumps ->
        let
          goDecider dcdr =
            case dcdr of
              Leaf (Inline expr) ->
                  Leaf (Inline (go expr))

              Leaf (Jump _target)  ->
                  dcdr

              Chain chain success failure ->
                  Chain chain (goDecider success) (goDecider failure)

              FanOut path tests fallback ->
                  FanOut path (map (second goDecider) tests) (goDecider fallback)
        in
          Case exprName (goDecider decider) (map (second go) jumps)

    Data tag values ->
        Data tag (map go values)

    DataAccess root index ->
        DataAccess (go root) index

    Access record field ->
        Access (go record) field

    Update record fields ->
        Update (go record) (map (second go) fields)

    Record fields ->
        Record (map (second go) fields)

    Cmd _ ->
        expression

    Sub _ ->
        expression

    OutgoingPort _ _ ->
        expression

    IncomingPort _ _ ->
        expression

    Program kind expr ->
        Program kind (go expr)

    GLShader _ _ _ ->
        expression

    Crash home region maybeBranchProblem ->
        Crash home region (fmap go maybeBranchProblem)
