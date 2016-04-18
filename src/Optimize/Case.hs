{-# OPTIONS_GHC -Wall #-}
module Optimize.Case (optimize) where

import Control.Arrow (second)
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Maybe as Maybe

import qualified AST.Expression.Optimized as Opt
import qualified AST.Pattern as P
import qualified Optimize.DecisionTree as DT



-- OPTIMIZE A CASE EXPRESSION


optimize :: DT.VariantDict -> String -> [(P.Canonical, Opt.Expr)] -> Opt.Expr
optimize variantDict exprName optBranches =
  let
    (patterns, indexedBranches) =
      unzip (zipWith indexify [0..] optBranches)

    decisionTree =
      DT.compile variantDict patterns
  in
    treeToExpr exprName decisionTree indexedBranches


indexify :: Int -> (a,b) -> ((a,Int), (Int,b))
indexify index (pattern, branch) =
    ( (pattern, index)
    , (index, branch)
    )



-- CONVERT A TREE TO AN EXPRESSION


treeToExpr :: String -> DT.DecisionTree -> [(Int, Opt.Expr)] -> Opt.Expr
treeToExpr name decisionTree allJumps =
  let
    decider =
        treeToDecider decisionTree

    targetCounts =
        countTargets decider

    (choices, maybeJumps) =
        unzip (map (createChoices targetCounts) allJumps)
  in
      Opt.Case
        name
        (insertChoices (Map.fromList choices) decider)
        (Maybe.catMaybes maybeJumps)



-- TREE TO DECIDER
--
-- Decision trees may have some redundancies, so we convert them to a Decider
-- which has special constructs to avoid code duplication when possible.


treeToDecider :: DT.DecisionTree -> Opt.Decider Int
treeToDecider tree =
  case tree of
    DT.Match target ->
        Opt.Leaf target

    -- zero options
    DT.Decision _ [] Nothing ->
        error "compiler bug, somehow created an empty decision tree"

    -- one option
    DT.Decision _ [(_, subTree)] Nothing ->
        treeToDecider subTree

    DT.Decision _ [] (Just subTree) ->
        treeToDecider subTree

    -- two options
    DT.Decision path [(test, successTree)] (Just failureTree) ->
        toChain path test successTree failureTree

    DT.Decision path [(test, successTree), (_, failureTree)] Nothing ->
        toChain path test successTree failureTree

    -- many options
    DT.Decision path edges Nothing ->
        let
          (necessaryTests, fallback) =
              (init edges, snd (last edges))
        in
          Opt.FanOut
            path
            (map (second treeToDecider) necessaryTests)
            (treeToDecider fallback)

    DT.Decision path edges (Just fallback) ->
        Opt.FanOut path (map (second treeToDecider) edges) (treeToDecider fallback)


toChain :: DT.Path -> DT.Test -> DT.DecisionTree -> DT.DecisionTree -> Opt.Decider Int
toChain path test successTree failureTree =
  let
    failure =
      treeToDecider failureTree
  in
    case treeToDecider successTree of
      Opt.Chain testChain success subFailure | failure == subFailure ->
          Opt.Chain ((path, test) : testChain) success failure

      success ->
          Opt.Chain [(path, test)] success failure



-- INSERT CHOICES
--
-- If a target appears exactly once in a Decider, the corresponding expression
-- can be inlined. Whether things are inlined or jumps is called a "choice".


countTargets :: Opt.Decider Int -> Map.Map Int Int
countTargets decisionTree =
  case decisionTree of
    Opt.Leaf target ->
        Map.singleton target 1

    Opt.Chain _ success failure ->
        Map.unionWith (+) (countTargets success) (countTargets failure)

    Opt.FanOut _ tests fallback ->
        Map.unionsWith (+) (map countTargets (fallback : map snd tests))


createChoices
    :: Map.Map Int Int
    -> (Int, Opt.Expr)
    -> ( (Int, Opt.Choice), Maybe (Int, Opt.Expr) )
createChoices targetCounts (target, branch) =
    if targetCounts ! target == 1 then
        ( (target, Opt.Inline branch)
        , Nothing
        )

    else
        ( (target, Opt.Jump target)
        , Just (target, branch)
        )


insertChoices
    :: Map.Map Int Opt.Choice
    -> Opt.Decider Int
    -> Opt.Decider Opt.Choice
insertChoices choiceDict decider =
  let
    go =
      insertChoices choiceDict
  in
    case decider of
      Opt.Leaf target ->
          Opt.Leaf (choiceDict ! target)

      Opt.Chain testChain success failure ->
          Opt.Chain testChain (go success) (go failure)

      Opt.FanOut path tests fallback ->
          Opt.FanOut path (map (second go) tests) (go fallback)

