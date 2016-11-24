{-# OPTIONS_GHC -Wall #-}
module Nitpick.PatternMatches (patternMatches) where

{- The algorithm used here comes from "Warnings for Pattern Matching"
by Luc Maranget. Check it out for more information!

http://moscova.inria.fr/~maranget/papers/warn/warn.pdf

-}

import Control.Arrow ((***), second)
import qualified Data.Foldable as F
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified AST.Expression.Canonical as C
import qualified AST.Helpers as Help
import qualified AST.Literal as L
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as Pattern
import qualified AST.Variable as Var
import Elm.Utils ((|>))
import Nitpick.Pattern (Pattern(..), fromCanonicalPattern)
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Pattern as Error
import qualified Reporting.Region as Region
import qualified Reporting.Result as Result



-- NITPICK PATTERN MATCHES


type Result warning a =
  Result.Result () warning Error.Error a


patternMatches :: Module.Interfaces -> Module.Canonical -> Result wrn DT.VariantDict
patternMatches interfaces (Module.Module name _ info) =
  let
    arityDict =
      toArityDict interfaces name (Module.unions info)

    variantDict =
      Map.map (Map.map _size) arityDict
  in
    pure variantDict
      <* checkExpression arityDict (Module.program info)


checkExpression :: ArityDict -> C.Expr -> Result wrn ()
checkExpression arityDict (A.A region expression) =
  let
    go =
      checkExpression arityDict

    go2 a b =
      go a <* go b
  in
  case expression of
    C.Literal _ ->
        Result.ok ()

    C.Var _ ->
        Result.ok ()

    C.List listExprs ->
        F.traverse_ go listExprs

    C.Binop _ leftExpr rightExpr ->
        go2 leftExpr rightExpr

    C.Lambda pattern@(A.A patRegion _) body ->
        checkPatterns arityDict patRegion Error.Arg [pattern]
        <* go body

    C.App func arg ->
        go2 func arg

    C.If branches finally ->
        F.traverse_ (uncurry go2) branches
        <* go finally

    C.Let defs body ->
        go body
          <* F.traverse_ goDef defs
      where
        goDef (C.Def _ pattern@(A.A patRegion _) expr _) =
            checkPatterns arityDict patRegion Error.LetBound [pattern]
            <* go expr

    C.Case expr branches ->
        go expr
        <* checkPatterns arityDict region Error.Case (map fst branches)
        <* F.traverse_ (go . snd) branches

    C.Ctor _ctor exprs ->
        F.traverse_ go exprs

    C.Access record _field ->
        go record

    C.Update record fields ->
        go record
        <* F.traverse_ (go . snd) fields

    C.Record fields ->
        F.traverse_ (go . snd) fields

    C.Cmd _ ->
        Result.ok ()

    C.Sub _ ->
        Result.ok ()

    C.OutgoingPort _ _ ->
        Result.ok ()

    C.IncomingPort _ _ ->
        Result.ok ()

    C.Program _ _ ->
        error "DANGER - Program AST nodes should not be in Nitpick.PatternMatches."

    C.SaveEnv _ _ ->
        Result.ok ()

    C.GLShader _ _ _ ->
        Result.ok ()



-- CHECK PATTERNS


checkPatterns :: ArityDict -> Region.Region -> Error.Origin -> [Pattern.Canonical] -> Result wrn ()
checkPatterns arityDict region origin patterns =
  do  matrix <- checkRedundant arityDict region [] patterns
      if isUseful arityDict matrix [Anything]
        then Result.throw region (Error.Incomplete origin (error "TODO"))
        else Result.ok ()


checkRedundant
  :: ArityDict
  -> Region.Region
  -> [[Pattern]]
  -> [Pattern.Canonical]
  -> Result wrn [[Pattern]]
checkRedundant arityDict region checked unchecked =
  case unchecked of
    [] ->
      Result.ok checked

    nextPattern@(A.A patRegion _) : rest ->
      let
        next =
          [fromCanonicalPattern nextPattern]
      in
        if isUseful arityDict checked next then
          checkRedundant arityDict region (next : checked) rest
        else
          Result.throw region (Error.Redundant patRegion (length checked + 1))



-- MISSING PATTERNS


--missingPatterns :: [[Pattern]] -> Int -> [Pattern]
--missingPatterns matrix n =
--  case matrix of
--    [] ->




-- DETECT USEFUL PATTERNS


isUseful :: ArityDict -> [[Pattern]] -> [Pattern] -> Bool
isUseful arityDict matrix vector =
  case (matrix, vector) of
    ([], _) ->
      True

    (_, []) ->
      False

    (_, firstPattern : patterns) ->
      case firstPattern of
        Ctor name args ->
          let
            newMatrix =
              Maybe.mapMaybe (specialize name (length args)) matrix
          in
            isUseful arityDict newMatrix (args ++ patterns)

        Anything ->
          case isComplete arityDict matrix of
            Nothing ->
              isUseful arityDict (Maybe.mapMaybe toDefault matrix) patterns

            Just arityInfo ->
              let
                isUsefulHelp (ctor, arity) =
                  isUseful
                    arityDict
                    (Maybe.mapMaybe (specialize ctor arity) matrix)
                    (replicate arity Anything ++ patterns)
              in
                any isUsefulHelp arityInfo

        Literal literal ->
          let
            newMatrix =
              Maybe.mapMaybe (specializeLiteral literal) matrix
          in
            isUseful arityDict newMatrix patterns



-- SPECIALIZE MATRICES


specialize :: Var.Canonical -> Int -> [Pattern] -> Maybe [Pattern]
specialize ctorName arity row =
  case row of
    [] ->
      error "Compiler error! Empty matrices should not get specialized."

    firstPattern : patterns ->
      case firstPattern of
        Ctor name args ->
          if name == ctorName then Just (args ++ patterns) else Nothing

        Anything ->
          Just (replicate arity Anything ++ patterns)

        Literal _ ->
          error $
            "Compiler bug! After type checking, constructors and literals\
            \ should never align in pattern match exhaustiveness checks."


specializeLiteral :: L.Literal -> [Pattern] -> Maybe [Pattern]
specializeLiteral literal row =
  case row of
    [] ->
      error "Compiler error! Empty matrices should not get specialized."

    firstPattern : patterns ->
      case firstPattern of
        Literal lit ->
          if lit == literal then Just patterns else Nothing

        Anything ->
          Just patterns

        Ctor _ _ ->
          error $
            "Compiler bug! After type checking, constructors and literals\
            \ should never align in pattern match exhaustiveness checks."


toDefault :: [Pattern] -> Maybe [Pattern]
toDefault row =
  case row of
    [] ->
      Nothing

    Ctor _ _ : _ ->
      Nothing

    Anything : patterns ->
      Just patterns

    Literal _ : _ ->
      Nothing



-- ALL CONSTRUCTORS ARE PRESENT?


isComplete :: ArityDict -> [[Pattern]] -> Maybe [(Var.Canonical, Int)]
isComplete arityDict matrix =
  let
    ctorSet =
      List.foldl' isCompleteHelp Set.empty matrix

    actual =
      Set.size ctorSet
  in
    if actual == 0 then
      Nothing

    else
      let
        (ArityInfo expected info) =
          getArityInfo (Set.findMin ctorSet) arityDict
      in
        if expected == actual then Just info else Nothing


isCompleteHelp :: Set.Set Var.Canonical -> [Pattern] -> Set.Set Var.Canonical
isCompleteHelp ctors row =
  case row of
    Ctor name _ : _ ->
      Set.insert name ctors

    _ ->
      ctors



-- ARITY DICT


type ArityDict =
  Map.Map Var.Home (Map.Map String ArityInfo)


data ArityInfo =
  ArityInfo
    { _size :: !Int
    , _info :: [(Var.Canonical, Int)]
    }


toArityDict :: Module.Interfaces -> ModuleName.Canonical -> Module.Unions -> ArityDict
toArityDict interfaces localName localUnions =
  interfaces
    |> Map.mapWithKey (\name iface -> toArityEntry name (Module.iUnions iface))
    |> Map.mapKeysMonotonic Var.Module
    |> Map.insert (Var.Module localName) (toArityEntry localName localUnions)
    |> Map.union builtinDict


builtinDict :: ArityDict
builtinDict =
  let
    listInfo =
      ArityInfo 2 [ (Var.builtin "::", 2), (Var.builtin "[]", 0) ]

    boolInfo =
      ArityInfo 2 [ (Var.builtin "True", 0), (Var.builtin "False", 0) ]
  in
    Map.singleton Var.BuiltIn $ Map.fromList $
      [ ("::", listInfo)
      , ("[]", listInfo)
      , ("True", boolInfo)
      , ("False", boolInfo)
      ]
      ++ map makeTupleInfo [0..8]


makeTupleInfo :: Int -> ( String, ArityInfo )
makeTupleInfo n =
  let
    name =
      "_Tuple" ++ show n
  in
    ( name, ArityInfo 1 [ (Var.builtin name, n) ] )


toArityEntry :: ModuleName.Canonical -> Module.Unions -> Map.Map String ArityInfo
toArityEntry name unions =
  Map.fromList (concatMap (toArityEntryHelp name) (Map.elems unions))


toArityEntryHelp :: ModuleName.Canonical -> Module.UnionInfo String -> [(String, ArityInfo)]
toArityEntryHelp name (_tvars, ctors) =
  let
    arityInfo =
      ArityInfo (length ctors) (map (Var.fromModule name *** length) ctors)
  in
    map (second (\_ -> arityInfo)) ctors


getArityInfo :: Var.Canonical -> ArityDict -> ArityInfo
getArityInfo var@(Var.Canonical home name) arityDict =
  case Map.lookup name =<< Map.lookup home arityDict of
    Just arityInfo ->
      arityInfo

    Nothing ->
      if Help.isTuple name then
        ArityInfo 1 [ (var, read (drop 6 name)) ]
      else
        error
          "Since the Nitpick phase happens after canonicalization and type\
          \ inference, it is impossible that a pattern in a case cannot be\
          \ found."