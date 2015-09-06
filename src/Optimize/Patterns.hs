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
    -> DT.DecisionTree DT.Jump
    -> [(Int, Opt.Expr)]
    -> Opt.Expr
inlinedCase name decisionTree jumpTargets =
  let
    substitutionDict =
        gatherSubstitutions decisionTree

    (jumps, sharedBranches) =
        unzip (map (toJumps substitutionDict) jumpTargets)
  in
      Opt.Case
        name
        (inlineJumps (Map.fromList jumps) decisionTree)
        (Maybe.catMaybes sharedBranches)


gatherSubstitutions
    :: DT.DecisionTree DT.Jump
    -> Map.Map Int [[(String, DT.Path)]]
gatherSubstitutions decisionTree =
  case decisionTree of
    DT.Decision _ edges fallback ->
        map snd edges
          |> maybe id (:) fallback
          |> map gatherSubstitutions
          |> Map.unionsWith (++)

    DT.Match (DT.Jump target substitutions) ->
        Map.singleton target [substitutions]


toJumps
    :: Map.Map Int [[(String, DT.Path)]]
    -> (Int, Opt.Expr)
    -> ( (Int, Opt.Jump), Maybe (Int, Opt.Branch) )
toJumps substitutionDict (target, branch) =
    case substitutionDict ! target of
      [substitutions] ->
          ( (target, Opt.Inline (Opt.Branch substitutions branch))
          , Nothing
          )

      substitutions : _ ->
          ( (target, Opt.Jump target)
          , Just (target, Opt.Branch substitutions branch)
          )

      _ ->
          error "it is not possible for there to be no substitutions"


inlineJumps
    :: Map.Map Int Opt.Jump
    -> DT.DecisionTree DT.Jump
    -> DT.DecisionTree Opt.Jump
inlineJumps jumpDict decisionTree =
  let
    go =
      inlineJumps jumpDict
  in
  case decisionTree of
    DT.Decision test edges fallback ->
        DT.Decision test (map (second go) edges) (fmap go fallback)

    DT.Match (DT.Jump target _) ->
        DT.Match (jumpDict ! target)
