{-# OPTIONS_GHC -Wall #-}
module Optimize.Patterns (optimize) where

import Control.Arrow (second)
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Maybe as Maybe

import qualified AST.Expression.Optimized as Opt
import qualified AST.Pattern as P
import Elm.Utils ((|>))
import qualified Optimize.Environment as Env
import qualified Optimize.Patterns.DecisionTree as DT
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


optimize
    :: DT.VariantDict
    -> R.Region
    -> Opt.Expr
    -> [(P.CanonicalPattern, Opt.Expr)]
    -> Env.Optimizer Opt.Expr
optimize variantDict region optExpr optBranches =
    let
        indexify index (pattern, branch) =
            ( (pattern, index)
            , (index, branch)
            )
    in
    do  crashBranches <-
          case last optBranches of
            (A.A ann P.Anything, Opt.Call (Opt.Crash _ _) [arg]) ->
                do  name <- Env.freshName
                    let newPattern = A.A ann (P.Var name)
                    let newCall = Opt.Call (Opt.Crash region (Just name)) [arg]
                    return (init optBranches ++ [(newPattern, newCall)])

            _ ->
                return optBranches

        let (patterns, indexedBranches) =
              unzip (zipWith indexify [0..] crashBranches)

        let decisionTree =
              DT.compile variantDict patterns

        tempName <- Env.freshName

        return $
          Opt.Let
            [ Opt.Def Opt.dummyFacts tempName optExpr ]
            (inlinedCase tempName decisionTree indexedBranches)


inlinedCase
    :: String
    -> DT.DecisionTree Int
    -> [(Int, Opt.Expr)]
    -> Opt.Expr
inlinedCase name decisionTree jumpTargets =
  let
    targetCounts =
        countTargets decisionTree

    (decisions, sharedBranches) =
        unzip (map (toDecisions targetCounts) jumpTargets)
  in
      Opt.Case
        name
        (inlineDecisions (Map.fromList decisions) decisionTree)
        (Maybe.catMaybes sharedBranches)


countTargets :: DT.DecisionTree Int -> Map.Map Int Int
countTargets decisionTree =
  case decisionTree of
    DT.Decision _ edges fallback ->
        map snd edges
          |> maybe id (:) fallback
          |> map countTargets
          |> Map.unionsWith (+)

    DT.Match target ->
        Map.singleton target 1


toDecisions
    :: Map.Map Int Int
    -> (Int, Opt.Expr)
    -> ( (Int, Opt.Decision), Maybe (Int, Opt.Expr) )
toDecisions targetCounts (target, branch) =
    if targetCounts ! target == 1 then
        ( (target, Opt.Inline branch)
        , Nothing
        )

    else
        ( (target, Opt.Jump target)
        , Just (target, branch)
        )


inlineDecisions
    :: Map.Map Int Opt.Decision
    -> DT.DecisionTree Int
    -> DT.DecisionTree Opt.Decision
inlineDecisions jumpDict decisionTree =
  let
    go =
      inlineDecisions jumpDict
  in
  case decisionTree of
    DT.Decision test edges fallback ->
        DT.Decision test (map (second go) edges) (fmap go fallback)

    DT.Match target ->
        DT.Match (jumpDict ! target)
