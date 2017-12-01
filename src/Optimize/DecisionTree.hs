{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Optimize.DecisionTree
  ( DecisionTree(..)
  , compile
  , Path(..)
  , Test(..)
  )
  where


{- To learn more about how this works, definitely read through:

    "When Do Match-Compilation Heuristics Matter?"

by Kevin Scott and Norman Ramsey. The rough idea is that we start with a simple
list of patterns and expressions, and then turn that into a "decision tree"
that requires as few tests as possible to make it to a leaf. Read the paper, it
explains this extraordinarily well! We are currently using the same heuristics
as SML/NJ to get nice trees.
-}

import Control.Arrow (second)
import Control.Monad (liftM, liftM2)
import Data.Binary
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)

import qualified AST.Canonical as Can
import qualified Elm.Name as N
import qualified Reporting.Annotation as A



-- COMPILE CASES


{-| Users of this module will mainly interact with this function. It takes
some normal branches and gives out a decision tree that has "labels" at all
the leafs and a dictionary that maps these "labels" to the code that should
run.

If 2 or more leaves point to the same label, we need to do some tricks in JS to
make that work nicely. When is JS getting goto?! ;) That is outside the scope
of this module though.
-}
compile :: [(Can.Pattern, Int)] -> DecisionTree
compile rawBranches =
  let
    format (pattern, index) =
        Branch index [(Empty, pattern)]
  in
    toDecisionTree (map format rawBranches)



-- DECISION TREES


data DecisionTree
  = Match Int
  | Decision
      { _path :: Path
      , _edges :: [(Test, DecisionTree)]
      , _default :: Maybe DecisionTree
      }
  deriving (Eq)


data Test
  = IsCtor Int N.Name
  | IsInt Int
  | IsChr Text
  | IsStr Text
  deriving (Eq, Ord)


data Path
  = Index Int Path
  | Field N.Name Path
  | Empty
  | Alias
  deriving (Eq)



-- PATH HELPERS


add :: Path -> Path -> Path
add path finalLink =
  case path of
    Empty ->
        finalLink

    Alias ->
        error "nothing should be added to an alias path"

    Index index subpath ->
        Index index (add subpath finalLink)

    Field name subpath ->
        Field name (add subpath finalLink)


subPositions :: Path -> [Can.Pattern] -> [(Path, Can.Pattern)]
subPositions path patterns =
    zipWith
      (\index pattern -> (add path (Index index Empty), pattern))
      [0..]
      patterns



-- ACTUALLY BUILD DECISION TREES


data Branch =
  Branch
    { _goal :: Int
    , _patterns :: [(Path, Can.Pattern)]
    }


toDecisionTree :: [Branch] -> DecisionTree
toDecisionTree rawBranches =
  let
    branches =
        map flattenPatterns rawBranches
  in
  case checkForMatch branches of
    Just goal ->
        Match goal

    Nothing ->
        let
          path =
              pickPath branches

          (edges, fallback) =
              gatherEdges branches path

          decisionEdges =
              map (second toDecisionTree) edges
        in
          case (decisionEdges, fallback) of
            ([(_tag, decisionTree)], []) ->
                decisionTree

            (_, []) ->
                Decision path decisionEdges Nothing

            ([], _ : _) ->
                toDecisionTree fallback

            (_, _) ->
                Decision path decisionEdges (Just (toDecisionTree fallback))


isComplete :: [Test] -> Bool
isComplete tests =
  case head tests of
    IsCtor alts _ ->
        alts == length tests

    _ ->
        False



-- FLATTEN PATTERNS


{-| Flatten type aliases and use the VariantDict to figure out when a tag is
the only variant so we can skip doing any tests on it.
-}
flattenPatterns :: Branch -> Branch
flattenPatterns (Branch goal pathPatterns) =
  Branch goal (concatMap flatten pathPatterns)


flatten :: (Path, Can.Pattern) -> [(Path, Can.Pattern)]
flatten pathPattern@(path, A.At region pattern) =
  case pattern of
    Can.PVar _ ->
        [pathPattern]

    Can.PAnything ->
        [pathPattern]

    Can.PAlias realPattern alias ->
        (add path Alias, A.At region (Can.PVar alias))
        :
        flatten (path, realPattern)

    Can.PRecord _ ->
        [pathPattern]

    Can.PUnit ->
        [pathPattern]

    Can.PTuple a b Nothing ->
        concatMap flatten (subPositions path [a,b])

    Can.PTuple a b (Just c) ->
        concatMap flatten (subPositions path [a,b,c])

    Can.PCtor _ _ _ (Can.CtorAlts numAlts _) _ args ->
        if numAlts == 1 then
          concatMap flatten (subPositions path (map dearg args))
        else
          [pathPattern]

    Can.PList _ ->
        [pathPattern]

    Can.PCons _ _ ->
        [pathPattern]

    Can.PChr _ ->
        [pathPattern]

    Can.PStr _ ->
        [pathPattern]

    Can.PInt _ ->
        [pathPattern]


dearg :: Can.PatternCtorArg -> Can.Pattern
dearg (Can.PatternCtorArg _ _ pattern) =
  pattern



-- SUCCESSFULLY MATCH


{-| If the first branch has no more "decision points" we can finally take that
path. If that is the case we give the resulting label and a mapping from free
variables to "how to get their value". So a pattern like (Just (x,_)) will give
us something like ("x" => value.0.0)
-}
checkForMatch :: [Branch] -> Maybe Int
checkForMatch branches =
  case branches of
    Branch goal patterns : _ | all (not . needsTests . snd) patterns ->
        Just goal

    _ ->
        Nothing



-- GATHER OUTGOING EDGES


gatherEdges :: [Branch] -> Path -> ([(Test, [Branch])], [Branch])
gatherEdges branches path =
  let
    relevantTests =
        testsAtPath path branches

    allEdges =
        map (edgesFor path branches) relevantTests

    fallbacks =
        if isComplete relevantTests then
          []
        else
          filter (isIrrelevantTo path) branches
  in
    ( allEdges, fallbacks )



-- FIND RELEVANT TESTS


testsAtPath :: Path -> [Branch] -> [Test]
testsAtPath selectedPath branches =
  let
    allTests =
      Maybe.mapMaybe (testAtPath selectedPath) branches

    skipVisited test curr@(uniqueTests, visitedTests) =
        if Set.member test visitedTests then
            curr
        else
            ( test : uniqueTests
            , Set.insert test visitedTests
            )
  in
    fst (foldr skipVisited ([], Set.empty) allTests)


testAtPath :: Path -> Branch -> Maybe Test
testAtPath selectedPath (Branch _ pathPatterns) =
  case List.lookup selectedPath pathPatterns of
    Nothing ->
        Nothing

    Just (A.At _ pattern) ->
        case pattern of
          Can.PCtor _ _ _ (Can.CtorAlts numAlts _) name _ ->
              Just (IsCtor numAlts name)

          Can.PInt int ->
              Just (IsInt int)

          Can.PStr str ->
              Just (IsStr str)

          Can.PChr chr ->
              Just (IsChr chr)

          Can.PList [] ->
              Just nilTest

          Can.PList _ ->
              Just consTest

          Can.PCons _ _ ->
              Just consTest

          Can.PVar _ ->
              Nothing

          Can.PAlias _ _ ->
              error "aliases should never reach 'testAtPath' function"

          Can.PAnything ->
              Nothing

          Can.PUnit ->
              Nothing

          Can.PTuple _ _ _ ->
              Nothing

          Can.PRecord _ ->
              Nothing


{-# NOINLINE nilTest #-}
nilTest :: Test
nilTest = IsCtor 2 "[]"


{-# NOINLINE consTest #-}
consTest :: Test
consTest = IsCtor 2 "::"



-- BUILD EDGES


edgesFor :: Path -> [Branch] -> Test -> (Test, [Branch])
edgesFor path branches test =
  ( test
  , Maybe.mapMaybe (toRelevantBranch test path) branches
  )


toRelevantBranch :: Test -> Path -> Branch -> Maybe Branch
toRelevantBranch test path branch@(Branch goal pathPatterns) =
  case extract path pathPatterns of
    Just (start, A.At region pattern, end) ->
        case pattern of
          Can.PCtor _ _ _ (Can.CtorAlts numAlts _) name args ->
              if test == IsCtor numAlts name then
                  Just (Branch goal (start ++ subPositions path (map dearg args) ++ end))
              else
                  Nothing

          Can.PList [] ->
              if test == IsCtor 2 "[]" then
                  Just (Branch goal (start ++ end))
              else
                  Nothing

          Can.PList (hd:tl) ->
              if test == IsCtor 2 "::" then
                  let tl' = A.At region (Can.PList tl) in
                  Just (Branch goal (start ++ subPositions path [ hd, tl' ] ++ end))
              else
                  Nothing

          Can.PCons hd tl ->
              if test == IsCtor 2 "::" then
                  Just (Branch goal (start ++ subPositions path [hd,tl] ++ end))
              else
                  Nothing

          Can.PChr chr ->
              if test == IsChr chr then
                  Just (Branch goal (start ++ end))
              else
                  Nothing

          Can.PStr str ->
              if test == IsStr str then
                  Just (Branch goal (start ++ end))
              else
                  Nothing

          Can.PInt int ->
              if test == IsInt int then
                  Just (Branch goal (start ++ end))
              else
                  Nothing

          _ ->
              Just branch

    _ ->
        Just branch


extract
    :: Path
    -> [(Path, Can.Pattern)]
    -> Maybe ([(Path, Can.Pattern)], Can.Pattern, [(Path, Can.Pattern)])
extract selectedPath pathPatterns =
  case pathPatterns of
    [] ->
        Nothing

    first@(path, pattern) : rest ->
        if path == selectedPath then
            Just ([], pattern, rest)

        else
            case extract selectedPath rest of
              Nothing ->
                  Nothing

              Just (start, foundPattern, end) ->
                  Just (first : start, foundPattern, end)



-- FIND IRRELEVANT BRANCHES


isIrrelevantTo :: Path -> Branch -> Bool
isIrrelevantTo selectedPath (Branch _ pathPatterns) =
  case List.lookup selectedPath pathPatterns of
    Nothing ->
        True

    Just pattern ->
        not (needsTests pattern)


needsTests :: Can.Pattern -> Bool
needsTests (A.At _ pattern) =
  case pattern of
    Can.PVar _ ->
        False

    Can.PAnything ->
        False

    Can.PAlias _ _ ->
        error "aliases should never reach 'isIrrelevantTo' function"

    Can.PRecord _ ->
        False

    Can.PCtor _ _ _ _ _ _ ->
        True

    Can.PList _ ->
        True

    Can.PCons _ _ ->
        True

    Can.PUnit ->
        False

    Can.PTuple _ _ _ ->
        False

    Can.PChr _ ->
        True

    Can.PStr _ ->
        True

    Can.PInt _ ->
        True



-- PICK A PATH


pickPath :: [Branch] -> Path
pickPath branches =
  let
    allPaths =
      Maybe.mapMaybe isChoicePath (concatMap _patterns branches)
  in
    case bests (addWeights (smallDefaults branches) allPaths) of
      [path] ->
          path

      tiedPaths ->
          head (bests (addWeights (smallBranchingFactor branches) tiedPaths))


isChoicePath :: (Path, Can.Pattern) -> Maybe Path
isChoicePath (path, pattern) =
  if needsTests pattern then
      Just path
  else
      Nothing


addWeights :: (Path -> Int) -> [Path] -> [(Path, Int)]
addWeights toWeight paths =
  map (\path -> (path, toWeight path)) paths


bests :: [(Path, Int)] -> [Path]
bests allPaths =
  case allPaths of
    [] ->
      error "Cannot choose the best of zero paths. This should never happen."

    (headPath, headWeight) : weightedPaths ->
      let
        gatherMinimum acc@(minWeight, paths) (path, weight) =
          if weight == minWeight then
            (minWeight, path : paths)

          else if weight < minWeight then
            (weight, [path])

          else
            acc
      in
        snd (List.foldl' gatherMinimum (headWeight, [headPath]) weightedPaths)



-- PATH PICKING HEURISTICS


smallDefaults :: [Branch] -> Path -> Int
smallDefaults branches path =
  length (filter (isIrrelevantTo path) branches)


smallBranchingFactor :: [Branch] -> Path -> Int
smallBranchingFactor branches path =
  let
    (edges, fallback) =
      gatherEdges branches path
  in
    length edges + (if null fallback then 0 else 1)



-- BINARY


instance Binary Test where
  put test =
    case test of
      IsCtor a b -> putWord8 0 >> put a >> put b
      IsChr a    -> putWord8 1 >> put a
      IsStr a    -> putWord8 2 >> put a
      IsInt a    -> putWord8 3 >> put a

  get =
    do  word <- getWord8
        case word of
          0 -> liftM2 IsCtor get get
          1 -> liftM  IsChr get
          2 -> liftM  IsStr get
          3 -> liftM  IsInt get
          _ -> error "problem getting DecisionTree.Test binary"


instance Binary Path where
  put path =
    case path of
      Index a b -> putWord8 0 >> put a >> put b
      Field a b -> putWord8 1 >> put a >> put b
      Empty     -> putWord8 2
      Alias     -> putWord8 3

  get =
    do  word <- getWord8
        case word of
          0 -> liftM2 Index get get
          1 -> liftM2 Field get get
          2 -> pure Empty
          3 -> pure Alias
          _ -> error "problem getting DecisionTree.Path binary"
