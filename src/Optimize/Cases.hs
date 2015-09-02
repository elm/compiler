module Optimize.Cases where
{- To learn more about how this works, definitely read through:

    "When Do Match-Compilation Heuristics Matter?"

by Kevin Scott and Norman Ramsey. The rough idea is that we start with a simple
list of patterns and expressions, and then turn that into a "decision tree"
that requires as few tests as possible to make it to a leaf. Read the paper, it
explains this extraordinarily well! We are currently using the same heuristics
as SML/NJ to get nice trees.

Users of this module will mainly interact with the 'compile' function which
takes some normal branches and gives out a decision tree that has "labels" at
all the leafs and a dictionary that maps these "labels" to the code that
should run.

If 2 or more leaves point to the same label, we need to do some tricks in JS to
make that work nicely. When is JS getting goto?! ;)
-}


import Control.Arrow (second)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Maybe as Maybe

import qualified AST.Expression.Optimized as Opt
import qualified AST.Literal as L
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A


-- COMPILE CASES

compile
    :: VariantDict
    -> [(OPattern, Opt.Expr)]
    -> (DecisionTree Int, Map.Map Int Opt.Expr)
compile variantDict rawBranches =
  let
    format index (pattern, expr) =
        ( Branch index [(Empty, pattern)]
        , (index, expr)
        )

    (branches, resultList) =
        unzip (zipWith format [0..] rawBranches)
  in
    ( toDecisionTree variantDict branches
    , Map.fromList resultList
    )


-- DECISION TREES

data DecisionTree a
    = Test
        { _test :: Path
        , _edges :: [(Tag, DecisionTree a)]
        , _default :: Maybe (DecisionTree a)
        }
    | Match
        { result :: a
        , substitutions :: Map.Map String Path
        }


-- EDGE CHECKS

data Tag
    = Constructor Var.Canonical
    | Literal L.Literal
    deriving (Eq, Ord)


-- PATHS

data Path
    = Position Int Path
    | Field String Path
    | Empty
    | Alias
    deriving (Eq)


add :: Path -> Path -> Path
add path finalLink =
  case path of
    Empty ->
        finalLink

    Alias ->
        error "nothing should be added to an alias path"

    Position index subpath ->
        Position index (add subpath finalLink)

    Field name subpath ->
        Field name (add subpath finalLink)


subPositions :: Path -> [OPattern] -> [(Path, OPattern)]
subPositions path patterns =
    zipWith
      (\index pattern -> (add path (Position index Empty), pattern))
      [0..]
      patterns


-- CREATE DECISION TREES


data Branch =
  Branch
    { _goal :: Int
    , _patterns :: [(Path, OPattern)]
    }


type OPattern = P.Optimized


type VariantDict = Map.Map Var.Canonical Int


-- BUILD A DECISION TREE

toDecisionTree :: VariantDict -> [Branch] -> DecisionTree Int
toDecisionTree variantDict rawBranches =
  let
    branches =
        map (skipUselessPatterns variantDict) rawBranches
  in
  case isAllValiables branches of
    Just (goal, substitutions) ->
        Match goal substitutions

    Nothing ->
        let
          path =
              smallDefaults branches

          relevantTags =
              tagsAtPath path branches

          allRawEdges =
              map (edgesFor path branches) relevantTags

          (rawEdges, unexplored) =
              if isComplete variantDict relevantTags then
                  ( init allRawEdges
                  , snd (last allRawEdges)
                  )

              else
                  ( allRawEdges
                  , filter (isIrrelevantTo path) branches
                  )

          edges =
              map (second (toDecisionTree variantDict)) rawEdges
        in
          case (edges, unexplored) of
            ([(_tag, decisionTree)], []) ->
                decisionTree

            (_, []) ->
                Test path edges Nothing

            (_, _) ->
                Test path edges (Just (toDecisionTree variantDict unexplored))


isComplete :: VariantDict -> [Tag] -> Bool
isComplete variantDict tags =
  case head tags of
    Constructor name ->
        (variantDict ! name) == length tags

    Literal (L.Boolean _) ->
        length tags == 2

    _ ->
        False



-- SKIP USELESS TESTS

skipUselessPatterns :: VariantDict -> Branch -> Branch
skipUselessPatterns variantDict (Branch goal pathPatterns) =
  Branch goal (concatMap (skipper variantDict) pathPatterns)


skipper :: VariantDict -> (Path, OPattern) -> [(Path, OPattern)]
skipper variantDict pathPattern@(path, A.A ann pattern) =
  case pattern of
    P.Var _ ->
        [pathPattern]

    P.Anything ->
        [pathPattern]

    P.Alias alias realPattern ->
        [ (add path Alias, A.A ann (P.Var alias))
        , (path, realPattern)
        ]

    P.Record _ ->
        [pathPattern]

    P.Data tag patterns ->
        if variantDict ! tag == 1 then
            concatMap (skipper variantDict) (subPositions path patterns)

        else
            [pathPattern]

    P.Literal _ ->
        [pathPattern]


-- VARIABLE SUBSTITUTIONS

isAllValiables :: [Branch] -> Maybe (Int, Map.Map String Path)
isAllValiables branches =
  case branches of
    Branch goal patterns : _ ->
        do  subs <- concat `fmap` mapM getSubstitution patterns
            return (goal, Map.fromList subs)

    _ ->
        Nothing


getSubstitution :: (Path, OPattern) -> Maybe [(String, Path)]
getSubstitution (path, A.A _ pattern) =
  case pattern of
    P.Var x ->
        Just [(x, path)]

    P.Anything ->
        Just []

    P.Alias _ _ ->
        error "aliases should never reach 'getSubstitution' function"

    P.Record fields ->
        Just (map (\name -> (name, add path (Field name Empty))) fields)

    P.Data _ _ ->
        Nothing

    P.Literal _ ->
        Nothing


-- FIND RELEVANT TAGS

tagsAtPath :: Path -> [Branch] -> [Tag]
tagsAtPath selectedPath branches =
  List.nub $ Maybe.mapMaybe (tagAtPath selectedPath) branches


tagAtPath :: Path -> Branch -> Maybe Tag
tagAtPath selectedPath (Branch _ pathPatterns) =
  case List.lookup selectedPath pathPatterns of
    Nothing ->
        Nothing

    Just (A.A _ pattern) ->
        case pattern of
          P.Data name _ ->
              Just (Constructor name)

          P.Literal lit ->
              Just (Literal lit)

          P.Var _ ->
              Nothing

          P.Alias _ _ ->
              error "aliases should never reach 'tagAtPath' function"

          P.Anything ->
              Nothing

          P.Record _ ->
              Nothing


-- BUILD EDGES

edgesFor :: Path -> [Branch] -> Tag -> (Tag, [Branch])
edgesFor path branches tag =
  ( tag
  , Maybe.mapMaybe (subBranch tag path) branches
  )


subBranch :: Tag -> Path -> Branch -> Maybe Branch
subBranch tag path branch@(Branch goal pathPatterns) =
  case extract path pathPatterns of
    Just (start, A.A _ pattern, end) ->
        case pattern of
          P.Data name patterns | tag == Constructor name ->
              Just (Branch goal (start ++ subPositions path patterns ++ end))

          P.Literal lit | tag == Literal lit ->
              Just (Branch goal (start ++ end))

          _ ->
              Nothing

    _ ->
        Just branch


extract
    :: Path
    -> [(Path, OPattern)]
    -> Maybe ([(Path, OPattern)], OPattern, [(Path, OPattern)])
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

    Just (A.A _ pattern) ->
        case pattern of
          P.Var _ ->
              True

          P.Anything ->
              True

          P.Alias _ _ ->
              error "aliases should never reach 'isIrrelevantTo' function"

          P.Record _ ->
              True

          P.Data _ _ ->
              False

          P.Literal _ ->
              False


-- PATH PICKING HEURISTICS

smallDefaults :: [Branch] -> Path
smallDefaults branches =
  let
    allPaths =
      Maybe.mapMaybe isChoicePath (concatMap _patterns branches)

    weightedPaths =
      map (\path -> (path, length (filter (isIrrelevantTo path) branches))) allPaths
  in
      fst (List.minimumBy (compare `on` snd) weightedPaths)


isChoicePath :: (Path, OPattern) -> Maybe Path
isChoicePath (path, A.A _ pattern) =
  case pattern of
    P.Var _ ->
        Nothing

    P.Anything ->
        Nothing

    P.Alias _ _ ->
        error "aliases should never reach 'isChoicePath' function"

    P.Record _ ->
        Nothing

    P.Data _ _ ->
        Just path

    P.Literal _ ->
        Just path


-- smallBranchingFactor :: [Branch] -> Path


