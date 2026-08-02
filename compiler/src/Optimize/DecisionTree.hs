{-# OPTIONS_GHC -fno-warn-x-partial #-}
{-# LANGUAGE ExtendedLiterals, MagicHash, OverloadedStrings, TemplateHaskell #-}
module Optimize.DecisionTree
  ( DecisionTree(..)
  , compile
  , Path(..)
  , Test(..)
  --
  , eTest, dTest
  , ePath, dPath
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
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.Set as Set

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Crash

import qualified AST.Canonical as Can
import qualified Data.Index as Index
import qualified Data.Utf8 as Utf8
import qualified Elm.ModuleName as ModuleName
import qualified Elm.String as ES
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
  = IsCtor ModuleName.Canonical Name.Name Index.ZeroBased Int Can.CtorOpts
  | IsCons
  | IsNil
  | IsTuple
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
            $(Crash.crash 'testAtPath) "aliases should never reach 'testAtPath' function"



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
        $(Crash.crash 'needsTests) "aliases should never reach 'isIrrelevantTo' function"




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
      $(Crash.crash 'bests) "Cannot choose the best of zero paths. This should never happen."

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


eTest :: Test -> E.Builder
eTest test =
  case test of
    IsCtor h n i a o -> E.u8# 0#Word8 <> ModuleName.eCanonical h <> Utf8.encode8 n <> Index.eZeroBased i <> E.int a <> Can.eCtorOpts o
    IsCons           -> E.u8# 1#Word8
    IsNil            -> E.u8# 2#Word8
    IsTuple          -> E.u8# 3#Word8
    IsChr c          -> E.u8# 4#Word8 <> E.char c
    IsStr s          -> E.u8# 5#Word8 <> ES.encode s
    IsInt i          -> E.u8# 6#Word8 <> E.int i
    IsBool b         -> E.u8# 7#Word8 <> E.bool b


dTest :: D.Decoder Test
dTest =
  do  tag <- D.u8
      case tag of
        0 -> liftM5 IsCtor ModuleName.dCanonical Utf8.decode8 Index.dZeroBased D.int Can.dCtorOpts
        1 -> pure   IsCons
        2 -> pure   IsNil
        3 -> pure   IsTuple
        4 -> liftM  IsChr D.char
        5 -> liftM  IsStr ES.decode
        6 -> liftM  IsInt D.int
        7 -> liftM  IsBool D.bool
        _ -> D.expecting "DecisionTree.Test"


ePath :: Path -> E.Builder
ePath path =
  case path of
    Index i p -> E.u8# 0#Word8 <> Index.eZeroBased i <> ePath p
    Unbox   p -> E.u8# 1#Word8 <> ePath p
    Empty     -> E.u8# 2#Word8


dPath :: D.Decoder Path
dPath =
  do  tag <- D.u8
      case tag of
        0 -> liftM2 Index Index.dZeroBased dPath
        1 -> liftM Unbox dPath
        2 -> pure Empty
        _ -> D.expecting "DecisionTree.Path"




