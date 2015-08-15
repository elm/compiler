module Nitpick.PatternMatches (patternMatches) where

import Control.Applicative ((<$>), (<*))
import Control.Monad.Except (forM, liftM)
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified AST.Expression.General as E
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Module as Module
import qualified AST.Pattern as Pattern
import qualified AST.Variable as Var
import Elm.Utils ((|>))
import qualified Nitpick.Pattern as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Nitpick as Error
import qualified Reporting.Region as Region
import qualified Reporting.Result as Result
import qualified Reporting.Warning.Nitpick as Warning


patternMatches
    :: Module.Interfaces
    -> Module.CanonicalBody
    -> Result.Result Warning.Warning e ()
patternMatches interfaces body =
    checkExpression (toAdtDict interfaces) (Module.program body)


-- ADT DICT

type AdtDict =
  Map.Map Var.Home (Map.Map String (Module.AdtInfo String))


toAdtDict :: Module.Interfaces -> AdtDict
toAdtDict =
  let
    -- TODO
    builtinDict =
        Map.empty

    interfaceDict =
        interfaces
          |> Map.map Module.iAdts
          |> Map.mapKeysMonotonic Var.Module
  in
    Map.union builtinDict interfaceDict


-- TODO this is wrong
lookupOtherCtors
    :: Var.Canonical
    -> AdtDict
    -> [(Var.Canonical, [Type.Canonical])]
lookupOtherCtors (Var.Canonical home name) adtDict =
  case Map.lookup name =<< Map.lookup home adtDict of
    Nothing ->
        error "TODO"

    Just (_tvars, ctors) ->
        ctors


-- CHECK EXPRESSIONS

checkExpression
    :: AdtDict
    -> Canonical.Expr
    -> Result.Result Warning.Warning e ()
checkExpression adtDict (A.A region expression) =
  let
    go =
      checkExpression adtDict

    go2 a b =
      F.traverse_ go [a,b]

    goPattern =
      checkPatterns adtDict region
  in
  case expression of
    E.ExplicitList listExprs ->
        F.traverse_ go listExprs

    E.Binop _ leftExpr rightExpr ->
        go2 leftExpr rightExpr

    E.Lambda pattern body ->
        goPattern [pattern]
        <* go body

    E.App func arg ->
        go2 func arg

    E.MultiIf branches ->
        F.traverse_ (uncurry go2) branches

    E.Let defs body ->
        go body
          <* F.traverse_ goDef defs
      where
        goDef (Canonical.Definition pattern expr _) =
            goPattern [p]
            <* go expr

    E.Case expr branches ->
        go expr
        <* goPattern (map fst branches)
        <* F.traverse_ (go . snd) branches

    E.Data _ctor exprs ->
        F.traverse_ go exprs

    E.Access record _field ->
        go record

    E.Remove record _field ->
        go record

    E.Insert record _field value ->
        go2 record value

    E.Modify record fields ->
        go record
        <* F.traverse_ (go . snd) fields

    E.Record fields ->
        F.traverse_ (go . snd) fields


-- CHECK PATTERNS

checkPatterns
    :: AdtDict
    -> Region.Region
    -> [Pattern.CanonicalPattern]
    -> Result.Result Warning.Warning e ()
checkPatterns adtDict region patterns =
  checkPatternsHelp [P.Anything] patterns


checkPatternsHelp
    :: AdtDict
    -> Region.Region
    -> [P.Pattern]
    -> [Pattern.CanonicalPattern]
    -> Result.Result Warning.Warning e ()
checkPatternsHelp adtDict region unhandled patterns =
  case (unhandled, patterns) of
    ([], []) ->
        return ()

    (_:_, []) ->
        Result.warn region (Inexhaustive unhandled)

    (_, A.A region pattern : remainingPatterns) ->
        do  newUnhandled <- filterPatterns adtDict region pattern unhandled
            checkPatternsHelp adtDict newUnhandled remainingPatterns


filterPatterns
    :: AdtDict
    -> Region.Region
    -> P.Pattern
    -> [P.Pattern]
    -> Result.Result Warning.Warning e [P.Pattern]
filterPatterns adtDict region pattern unhandled =
  let
    nitPattern =
      P.fromPattern pattern

    noIntersection pat =
      intersection pat nitPattern == Nothing
  in
    if all noIntersection unhandled
      then
        do  Result.warn region Redundant
            return unhandled
      else
        do  let complementPatterns = complement adtDict nitPattern
            return $
              concatMap (\p -> Maybe.mapMaybe (intersection p) complementPatterns) unhandled


-- PATTERN INTERSECTION

intersection :: P.Pattern -> P.Pattern -> Maybe P.Pattern
intersection pattern1 pattern2 =
  case (pattern1, pattern2) of
    (P.Alias _ pattern1', _) ->
        intersection pattern1' pattern2

    (_, P.Alias _ pattern2') ->
        intersection pattern1 pattern2'

    (P.Anything, _) ->
        Just pattern2

    (P.Var _, _) ->
        Just pattern2

    (_, P.Anything) ->
        Just pattern1

    (_, P.Var _) ->
        Just pattern1

    (P.Data ctor1 args1, P.Data ctor2 args2) ->
        if ctor1 /= ctor2
          then Nothing
          else Data ctor1 <$> sequence (zipWith intersection args1 args2)

    -- TODO Assuming the fields are identical.
    (P.Record _fields, P.Record _) ->
        Just pattern1

    (P.Literal lit1, P.Literal lit2) ->
        if lit1 == lit2
          then Just pattern1
          else Nothing

    (P.AnythingBut literals, P.Literal lit) ->
        if Set.member lit literals
          then Nothing
          else Just pattern2

    (P.Literal lit, P.AnythingBut literals) ->
        if Set.membur lit literals
          then Nothing
          else Just pattern1

    (P.AnythingBut literals1, P.AnythingBut literals2) ->
        Just (AnythingBut (Set.union literals1 literals2))

    _ ->
        Nothing


-- PATTERN COMPLEMENT

complement :: AdtDict -> P.Pattern -> [P.Pattern]
complement adtDict nitPattern =
  case nitPattern of
    P.Record _fields ->
        []

    P.Alias _name pattern ->
        complement adtDict pattern

    P.Var _name ->
        []

    P.Anything ->
        []

    P.Literal lit ->
        [AnythingBut (Set.singleton lit)]

    P.AnythingBut literals ->
        map Literal (Set.toList literals)

    P.Data ctor patterns ->
        complementData adtDict ctor patterns


complementData :: AdtDict -> Var.Canonical -> [P.Pattern] -> [P.Pattern]
complementData adtDict canonicalTag patterns =
  let
    ctors =
        lookupOtherCtors ctor adtDict

    dataComplements =
        Maybe.mapMaybe (maybeData canonicalTag) ctors

    numArgs = length patterns

    argComplements =
        concat (zipWith (makeArgComplements adtDict numArgs) [0..] patterns)
  in
    dataComplements ++ argComplements


maybeData :: Var.Canonical -> (String, Module.AdtInfo) -> Maybe P.Pattern
maybeData (Var.Canonical home name) (tag, args) =
  if tag == name
    then
      Nothing
    else
      Just (P.Data (Var.Canonical home tag) (replicate (length args) P.Anything))


makeArgComplements :: AdtDict -> Int -> Int -> P.Pattern -> [P.Pattern]
makeArgComplements adtDict numArgs index argPattern =
  let
    complementList =
      complement adtDict argPattern

    paddedArgs pat =
      replicate index P.Anything ++ pat : replicate (numArgs - index - 1) P.Anything
  in
    map (P.Data ctor . paddedArgs) complementList
