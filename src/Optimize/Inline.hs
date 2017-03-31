{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Optimize.Inline (inline) where

import Control.Arrow (second, (***))
import Control.Monad (foldM)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)

import AST.Expression.Optimized (Expr(..), Decider(..), Choice(..))
import qualified AST.Expression.Optimized as Opt
import qualified Optimize.Environment as Env



inline :: Map.Map Text Opt.Expr -> Opt.Expr -> Env.Optimizer Opt.Expr
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
            Let (map (second Opt.Def) defs) inlinedExpr


data Subs =
  Subs
    { _substitutions :: Map.Map Text Opt.Expr
    , _definitions :: [(Text, Opt.Expr)]
    }


processSubs :: Subs -> (Text, (Int, Opt.Expr)) -> Env.Optimizer Subs
processSubs (Subs subs defs) (name, (n, expr)) =
  if n == 1 then
      return $ Subs (Map.insert name expr subs) defs

  else
      do  uniqueName <- Env.freshName
          return $ Subs
              (Map.insert name (Opt.VarLocal uniqueName) subs)
              ((uniqueName, expr) : defs)



-- HELPERS


deleteBatch :: [Text] -> Map.Map Text a -> Map.Map Text a
deleteBatch names dict =
  List.foldl' (flip Map.delete) dict names



-- COUNT VARIABLE APPEARANCES


count :: Opt.Expr -> Map.Map Text Int
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

    VarLocal name ->
        Map.singleton name 1

    VarGlobal _ ->
        Map.empty

    List exprs ->
        countMany exprs

    Binop _ left right ->
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
          countDef (name, def) =
            case def of
              Opt.Def body ->
                  Map.delete name (count body)

              Opt.TailDef argNames body ->
                  deleteBatch (name:argNames) (count body)

          exprCount =
            deleteBatch (map fst defs) (count expr)
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

    Ctor _tag values ->
        countMany values

    CtorAccess root _index ->
        count root

    Access record _field ->
        count record

    Update record fields ->
        Map.unionWith (+) (count record) (countMany (map snd fields))

    Record fields ->
        countMany (map snd fields)

    Cmd _ _ ->
        Map.empty

    Sub _ _ ->
        Map.empty

    OutgoingPort _ _ ->
        Map.empty

    IncomingPort _ _ ->
        Map.empty

    Program _ expr ->
        count expr

    GLShader _ ->
        Map.empty

    Crash _ _ maybeBranchProblem ->
        maybe Map.empty count maybeBranchProblem



-- REPLACE


replace :: Map.Map Text Opt.Expr -> Opt.Expr -> Opt.Expr
replace substitutions expression =
  let
    go = replace substitutions
  in
  case expression of
    Literal _ ->
        expression

    VarLocal name ->
        maybe expression id (Map.lookup name substitutions)

    VarGlobal _ ->
        expression

    List exprs ->
        List (map go exprs)

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
          replaceDef (name, def) =
            case def of
              Opt.Def body ->
                  (,) name $ Opt.Def $
                    replace (Map.delete name substitutions) body

              Opt.TailDef argNames body ->
                  (,) name $ Opt.TailDef argNames $
                    replace (deleteBatch (name:argNames) substitutions) body

          boundNames =
            map fst defs
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

    Ctor tag values ->
        Ctor tag (map go values)

    CtorAccess root index ->
        CtorAccess (go root) index

    Access record field ->
        Access (go record) field

    Update record fields ->
        Update (go record) (map (second go) fields)

    Record fields ->
        Record (map (second go) fields)

    Cmd _ _ ->
        expression

    Sub _ _ ->
        expression

    OutgoingPort _ _ ->
        expression

    IncomingPort _ _ ->
        expression

    Program kind expr ->
        Program kind (go expr)

    GLShader _ ->
        expression

    Crash home region maybeBranchProblem ->
        Crash home region (fmap go maybeBranchProblem)
