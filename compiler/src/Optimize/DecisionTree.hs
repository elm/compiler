{-# OPTIONS_GHC -fno-warn-x-partial #-}
{-# LANGUAGE ExtendedLiterals, MagicHash, OverloadedStrings, TemplateHaskell #-}
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
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Crash

import qualified AST.Canonical as Can
import qualified AST.Prim.Name as N
import qualified AST.Prim.Tag as Tag
import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Elm.String as ES
import qualified Nitpick.Utils as Nit
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
    format (pattern, index) = Branch index [(Empty, pattern)]
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
  = IsVariant ModuleName.Canonical N.Name Tag.Tag Nit.NumAlts
  | IsEnum    ModuleName.Canonical N.Name Tag.Tag Nit.NumAlts
  | IsCons
  | IsNil
  | IsInt Int
  | IsChr Char
  | IsStr ES.String
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
    branches = map flattenPatterns rawBranches
  in
  case checkForMatch branches of
    Just goal -> Match goal
    Nothing ->
      let
        path = pickPath branches
        (edges, fallback) = gatherEdges branches path
        decisionEdges = map (second toDecisionTree) edges
      in
      case (decisionEdges, fallback) of
        ([(_,dt)], [] ) -> dt
        (_       , [] ) -> Decision path decisionEdges Nothing
        ([]      , _:_) -> toDecisionTree fallback
        (_       , _  ) -> Decision path decisionEdges (Just (toDecisionTree fallback))


isComplete :: [Test] -> Bool
isComplete tests =
  case head tests of
    IsVariant _ _ _ (Nit.NumAlts n) -> n == length tests
    IsEnum    _ _ _ (Nit.NumAlts n) -> n == length tests
    IsCons                          -> length tests == 2
    IsNil                           -> length tests == 2
    IsChr _                         -> False
    IsStr _                         -> False
    IsInt _                         -> False
    IsBool _                        -> length tests == 2



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
    Can.PVar _              -> pathPattern : otherPathPatterns
    Can.PAnything           -> pathPattern : otherPathPatterns
    Can.PChr _              -> pathPattern : otherPathPatterns
    Can.PStr _              -> pathPattern : otherPathPatterns
    Can.PInt _              -> pathPattern : otherPathPatterns
    Can.PBool _ _ _ _       -> pathPattern : otherPathPatterns
    Can.PRecord _           -> pathPattern : otherPathPatterns
    Can.PList _             -> pathPattern : otherPathPatterns
    Can.PCons _ _           -> pathPattern : otherPathPatterns
    Can.PUnit               -> otherPathPatterns
    Can.PVariant _ _ _ _    -> pathPattern : otherPathPatterns
    Can.PEnum    _ _ _      -> pathPattern : otherPathPatterns
    Can.PVector  _ _ a b cs -> foldr flatten otherPathPatterns (subPositions path (map dearg (a:b:cs)))
    Can.PBox     _ _ _ p    -> flatten (Unbox path, p) otherPathPatterns
    Can.PVoid    _ _        -> otherPathPatterns

    Can.PPair a b ->
      flatten (Index Index.first  path, a) $
      flatten (Index Index.second path, b) otherPathPatterns

    Can.PTriple a b c ->
      flatten (Index Index.first  path, a) $
      flatten (Index Index.second path, b) $
      flatten (Index Index.third  path, c) otherPathPatterns

    Can.PAlias realPattern alias ->
      flatten (path, realPattern) $
        (path, A.At region (Can.PVar alias)) : otherPathPatterns


subPositions :: Path -> [Can.Pattern] -> [(Path, Can.Pattern)]
subPositions path patterns =
  Index.indexedMap (\index pattern -> (Index index path, pattern)) patterns


dearg :: Can.PArg -> Can.Pattern
dearg (Can.PArg _ _ pattern) =
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
    ( allEdges, fallbacks )
  where
    relevantTests = testsAtPath path branches
    allEdges = map (edgesFor path branches) relevantTests
    fallbacks =
      if isComplete relevantTests
      then []
      else filter (isIrrelevantTo path) branches



-- FIND RELEVANT TESTS


testsAtPath :: Path -> [Branch] -> [Test]
testsAtPath selectedPath branches =
  let
    allTests =
      Maybe.mapMaybe (testAtPath selectedPath) branches

    skipVisited test curr@(uniqueTests, visitedTests) =
        if Set.member test visitedTests
        then curr
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
        Can.PVariant n t tag _ -> withPType t $ \h numAlts -> Just (IsVariant h n tag numAlts)
        Can.PEnum    n t tag   -> withPType t $ \h numAlts -> Just (IsEnum    h n tag numAlts)
        Can.PVector  _ _ _ _ _ -> Nothing
        Can.PBox     _ _ _ _   -> Nothing
        Can.PVoid    _ _       -> Nothing
        Can.PList ps           -> Just (case ps of { [] -> IsNil ; _ -> IsCons })
        Can.PCons _ _          -> Just IsCons
        Can.PPair   _ _        -> Nothing
        Can.PTriple _ _ _      -> Nothing
        Can.PUnit              -> Nothing
        Can.PVar _             -> Nothing
        Can.PAnything          -> Nothing
        Can.PInt n             -> Just (IsInt n)
        Can.PStr s             -> Just (IsStr s)
        Can.PChr c             -> Just (IsChr c)
        Can.PBool b _ _ _      -> Just (IsBool b)
        Can.PRecord _          -> Nothing
        Can.PAlias _ _         -> $(Crash.crash 'testAtPath) "aliases should never reach 'testAtPath' function"
  where
    withPType (Can.PType h _ _ _ (Nit.Variants numAlts _)) cont =
      cont h numAlts



-- BUILD EDGES


edgesFor :: Path -> [Branch] -> Test -> (Test, [Branch])
edgesFor path branches test =
  ( test
  , Maybe.mapMaybe (toRelevantBranch test path) branches
  )


toRelevantBranch :: Test -> Path -> Branch -> Maybe Branch
toRelevantBranch test path branch@(Branch goal pathPatterns) =
  case extract path pathPatterns of
    Found start (A.At r pattern) end ->
        case pattern of
          Can.PVariant name _ _ ps   ->
            case test of
              IsVariant _ testName _ _ | name == testName ->
                Just $ Branch goal $ start ++ subPositions path (map dearg ps) ++ end

              _ ->
                Nothing

          Can.PEnum name _ _ ->
            case test of
              IsEnum _ testName _ _ | name == testName ->
                Just $ Branch goal $ start ++ end

              _ ->
                Nothing

          Can.PVector  _ _ a b cs -> Just $ Branch goal $ start ++ subPositions path (map dearg (a:b:cs)) ++ end
          Can.PBox     _ _ _ p    -> Just $ Branch goal $ start ++ [(Unbox path, p)] ++ end
          Can.PVoid    _ _        -> Just $ Branch goal $ start ++ end
          Can.PList []      -> case test of { IsNil  -> Just (Branch goal (start                                                 ++ end)) ; _ -> Nothing }
          Can.PList (p:ps)  -> case test of { IsCons -> Just (Branch goal (start ++ subPositions path [p, A.At r (Can.PList ps)] ++ end)) ; _ -> Nothing }
          Can.PCons  p ps   -> case test of { IsCons -> Just (Branch goal (start ++ subPositions path [p, ps]                    ++ end)) ; _ -> Nothing }
          Can.PChr c        -> case test of { IsChr  c' | c == c' -> Just (Branch goal (start ++ end)) ; _ -> Nothing }
          Can.PStr s        -> case test of { IsStr  s' | s == s' -> Just (Branch goal (start ++ end)) ; _ -> Nothing }
          Can.PInt n        -> case test of { IsInt  n' | n == n' -> Just (Branch goal (start ++ end)) ; _ -> Nothing }
          Can.PBool b _ _ _ -> case test of { IsBool b' | b == b' -> Just (Branch goal (start ++ end)) ; _ -> Nothing }
          Can.PUnit         -> Just $ Branch goal $ start ++ end
          Can.PPair   a b   -> Just $ Branch goal $ start ++ subPositions path [a,b]   ++ end
          Can.PTriple a b c -> Just $ Branch goal $ start ++ subPositions path [a,b,c] ++ end
          Can.PVar _        -> Just branch
          Can.PAnything     -> Just branch
          Can.PRecord _     -> Just branch
          Can.PAlias _ _    -> Just branch

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
        if path == selectedPath
        then Found [] pattern rest
        else
          case extract selectedPath rest of
            Found start foundPattern end -> Found (first : start) foundPattern end
            NotFound                     -> NotFound



-- FIND IRRELEVANT BRANCHES


isIrrelevantTo :: Path -> Branch -> Bool
isIrrelevantTo selectedPath (Branch _ pathPatterns) =
  case List.lookup selectedPath pathPatterns of
    Nothing -> True
    Just p  -> not (needsTests p)


needsTests :: Can.Pattern -> Bool
needsTests (A.At _ pattern) =
  case pattern of
    Can.PVar _             -> False
    Can.PAnything          -> False
    Can.PRecord _          -> False
    Can.PVariant _ _ _ _   -> True
    Can.PEnum    _ _ _     -> True
    Can.PVector  _ _ _ _ _ -> True
    Can.PBox     _ _ _ _   -> True
    Can.PVoid    _ _       -> False
    Can.PList _            -> True
    Can.PCons _ _          -> True
    Can.PUnit              -> False
    Can.PPair   _ _        -> True
    Can.PTriple _ _ _      -> True
    Can.PChr _             -> True
    Can.PStr _             -> True
    Can.PInt _             -> True
    Can.PBool _ _ _ _      -> True
    Can.PAlias _ _         -> $(Crash.crash 'needsTests) "aliases should never reach 'isIrrelevantTo' function"



-- PICK A PATH


pickPath :: [Branch] -> Path
pickPath branches =
  let
    allPaths = Maybe.mapMaybe isChoicePath (concatMap _patterns branches)
  in
  case bests (addWeights (smallDefaults branches) allPaths) of
    [path]    -> path
    tiedPaths -> head (bests (addWeights (smallBranchingFactor branches) tiedPaths))


isChoicePath :: (Path, Can.Pattern) -> Maybe Path
isChoicePath (path, pattern) =
  if needsTests pattern
  then Just path
  else Nothing


addWeights :: (Path -> Int) -> [Path] -> [(Path, Int)]
addWeights toWeight paths =
  map (\path -> (path, toWeight path)) paths


bests :: [(Path, Int)] -> [Path]
bests allPaths =
  case allPaths of
    [] ->
      $(Crash.crash 'bests) "Cannot choose the best of zero paths. This should never happen."

    (headPath, headWeight) : weightedPaths ->
      let
        gatherMinimum acc@(minWeight, paths) (path, weight)
          | weight == minWeight = (minWeight, path : paths)
          | weight <  minWeight = (weight, [path])
          | otherwise           = acc
      in
        snd (List.foldl' gatherMinimum (headWeight, [headPath]) weightedPaths)



-- PATH PICKING HEURISTICS


smallDefaults :: [Branch] -> Path -> Int
smallDefaults branches path =
  length (filter (isIrrelevantTo path) branches)


smallBranchingFactor :: [Branch] -> Path -> Int
smallBranchingFactor branches path =
    length edges + (if null fallback then 0 else 1)
  where
    (edges, fallback) = gatherEdges branches path

