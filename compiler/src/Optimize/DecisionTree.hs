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
import Control.Monad (liftM, liftM2, liftM5)
import Data.Binary
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Data.Index as Index
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
  = IsCtor ModuleName.Canonical N.Name Index.ZeroBased Int Can.CtorOpts
  | IsCons
  | IsNil
  | IsTuple
  | IsInt Int
  | IsChr Text
  | IsStr Text
  | IsBool Bool
  deriving (Eq, Ord)


data Path
  = Index Index.ZeroBased Path
  | Unbox Path
  | Empty
  deriving (Eq)



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
    IsCtor _ _ _ numAlts _ ->
      numAlts == length tests

    IsCons ->
      length tests == 2

    IsNil ->
      length tests == 2

    IsTuple ->
      True

    IsChr _ ->
      False

    IsStr _ ->
      False

    IsInt _ ->
      False

    IsBool _ ->
      length tests == 2



-- FLATTEN PATTERNS


{-| Flatten type aliases and use the VariantDict to figure out when a tag is
the only variant so we can skip doing any tests on it.
-}
flattenPatterns :: Branch -> Branch
flattenPatterns (Branch goal pathPatterns) =
  Branch goal (foldr flatten [] pathPatterns)


flatten :: (Path, Can.Pattern) -> [(Path, Can.Pattern)] -> [(Path, Can.Pattern)]
flatten pathPattern@(path, A.At region pattern) otherPathPatterns =
  case pattern of
    Can.PVar _ ->
      pathPattern : otherPathPatterns

    Can.PAnything ->
      pathPattern : otherPathPatterns

    Can.PCtor _ _ (Can.Union _ _ numAlts _) _ _ ctorArgs ->
      if numAlts == 1 then
        case map dearg ctorArgs of
          [arg] ->
            flatten (Unbox path, arg) otherPathPatterns

          args ->
            foldr flatten otherPathPatterns (subPositions path args)
      else
        pathPattern : otherPathPatterns

    Can.PTuple a b maybeC ->
      flatten (Index Index.first path, a) $
      flatten (Index Index.second path, b) $
        case maybeC of
          Nothing ->
            otherPathPatterns

          Just c ->
            flatten (Index Index.third path, c) otherPathPatterns

    Can.PUnit ->
      otherPathPatterns

    Can.PAlias realPattern alias ->
      flatten (path, realPattern) $
        (path, A.At region (Can.PVar alias)) : otherPathPatterns

    Can.PRecord _ ->
      pathPattern : otherPathPatterns

    Can.PList _ ->
      pathPattern : otherPathPatterns

    Can.PCons _ _ ->
      pathPattern : otherPathPatterns

    Can.PChr _ ->
      pathPattern : otherPathPatterns

    Can.PStr _ ->
      pathPattern : otherPathPatterns

    Can.PInt _ ->
      pathPattern : otherPathPatterns

    Can.PBool _ _ ->
      pathPattern : otherPathPatterns


subPositions :: Path -> [Can.Pattern] -> [(Path, Can.Pattern)]
subPositions path patterns =
  Index.indexedMap (\index pattern -> (Index index path, pattern)) patterns


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
        Can.PCtor home _ (Can.Union _ _ numAlts opts) name index _ ->
            Just (IsCtor home name index numAlts opts)

        Can.PList ps ->
            Just (case ps of { [] -> IsNil ; _ -> IsCons })

        Can.PCons _ _ ->
            Just IsCons

        Can.PTuple _ _ _ ->
            Just IsTuple

        Can.PUnit ->
            Just IsTuple

        Can.PVar _ ->
            Nothing

        Can.PAnything ->
            Nothing

        Can.PInt int ->
            Just (IsInt int)

        Can.PStr str ->
            Just (IsStr str)

        Can.PChr chr ->
            Just (IsChr chr)

        Can.PBool _ bool ->
            Just (IsBool bool)

        Can.PRecord _ ->
            Nothing

        Can.PAlias _ _ ->
            error "aliases should never reach 'testAtPath' function"



-- BUILD EDGES


edgesFor :: Path -> [Branch] -> Test -> (Test, [Branch])
edgesFor path branches test =
  ( test
  , Maybe.mapMaybe (toRelevantBranch test path) branches
  )


toRelevantBranch :: Test -> Path -> Branch -> Maybe Branch
toRelevantBranch test path branch@(Branch goal pathPatterns) =
  case extract path pathPatterns of
    Found start (A.At region pattern) end ->
        case pattern of
          Can.PCtor _ _ (Can.Union _ _ numAlts _) name _ ctorArgs ->
              case test of
                IsCtor _ testName _ _ _ | name == testName ->
                  Just $ Branch goal $
                    case map dearg ctorArgs of
                      [arg] | numAlts == 1 ->
                        start ++ [(Unbox path, arg)] ++ end

                      args ->
                        start ++ subPositions path args ++ end

                _ ->
                  Nothing

          Can.PList [] ->
              case test of
                IsNil ->
                  Just (Branch goal (start ++ end))

                _ ->
                  Nothing

          Can.PList (hd:tl) ->
              case test of
                IsCons ->
                  let tl' = A.At region (Can.PList tl) in
                  Just (Branch goal (start ++ subPositions path [ hd, tl' ] ++ end))

                _ ->
                  Nothing

          Can.PCons hd tl ->
              case test of
                IsCons ->
                  Just (Branch goal (start ++ subPositions path [hd,tl] ++ end))

                _ ->
                  Nothing

          Can.PChr chr ->
              case test of
                IsChr testChr | chr == testChr ->
                  Just (Branch goal (start ++ end))
                _ ->
                  Nothing

          Can.PStr str ->
              case test of
                IsStr testStr | str == testStr ->
                  Just (Branch goal (start ++ end))

                _ ->
                  Nothing

          Can.PInt int ->
              case test of
                IsInt testInt | int == testInt ->
                  Just (Branch goal (start ++ end))

                _ ->
                  Nothing

          Can.PBool _ bool ->
              case test of
                IsBool testBool | bool == testBool ->
                  Just (Branch goal (start ++ end))

                _ ->
                  Nothing

          Can.PUnit ->
              Just (Branch goal (start ++ end))

          Can.PTuple a b maybeC ->
              Just (Branch goal (start ++ subPositions path (a : b : Maybe.maybeToList maybeC) ++ end))

          Can.PVar _ ->
              Just branch

          Can.PAnything ->
              Just branch

          Can.PRecord _ ->
              Just branch

          Can.PAlias _ _ ->
              Just branch

    NotFound ->
        Just branch


data Extract
  = NotFound
  | Found [(Path, Can.Pattern)] Can.Pattern [(Path, Can.Pattern)]


extract :: Path -> [(Path, Can.Pattern)] -> Extract
extract selectedPath pathPatterns =
  case pathPatterns of
    [] ->
        NotFound

    first@(path, pattern) : rest ->
        if path == selectedPath then
            Found [] pattern rest

        else
            case extract selectedPath rest of
              NotFound ->
                  NotFound

              Found start foundPattern end ->
                  Found (first : start) foundPattern end



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
    Can.PVar _            -> False
    Can.PAnything         -> False
    Can.PRecord _         -> False
    Can.PCtor _ _ _ _ _ _ -> True
    Can.PList _           -> True
    Can.PCons _ _         -> True
    Can.PUnit             -> True
    Can.PTuple _ _ _      -> True
    Can.PChr _            -> True
    Can.PStr _            -> True
    Can.PInt _            -> True
    Can.PBool _ _         -> True
    Can.PAlias _ _ ->
        error "aliases should never reach 'isIrrelevantTo' function"




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
      IsCtor a b c d e -> putWord8 0 >> put a >> put b >> put c >> put d >> put e
      IsCons           -> putWord8 1
      IsNil            -> putWord8 2
      IsTuple          -> putWord8 3
      IsChr a          -> putWord8 4 >> put a
      IsStr a          -> putWord8 5 >> put a
      IsInt a          -> putWord8 6 >> put a
      IsBool a         -> putWord8 7 >> put a

  get =
    do  word <- getWord8
        case word of
          0 -> liftM5 IsCtor get get get get get
          1 -> pure   IsCons
          2 -> pure   IsNil
          3 -> pure   IsTuple
          4 -> liftM  IsChr get
          5 -> liftM  IsStr get
          6 -> liftM  IsInt get
          7 -> liftM  IsBool get
          _ -> error "problem getting DecisionTree.Test binary"


instance Binary Path where
  put path =
    case path of
      Index a b -> putWord8 0 >> put a >> put b
      Unbox a   -> putWord8 1 >> put a
      Empty     -> putWord8 2

  get =
    do  word <- getWord8
        case word of
          0 -> liftM2 Index get get
          1 -> liftM Unbox get
          2 -> pure Empty
          _ -> error "problem getting DecisionTree.Path binary"
