module Nitpick.PatternMatches (patternMatches) where

import Control.Applicative ((<$>), (<*))
import Control.Arrow (second)
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified AST.Expression.General as E
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Helpers as Help
import qualified AST.Module as Module
import qualified AST.Pattern as Pattern
import qualified AST.Variable as Var
import Elm.Utils ((|>))
import Nitpick.Pattern (Pattern(..), fromCanonicalPattern)
import qualified Reporting.Annotation as A
import qualified Reporting.Region as Region
import qualified Reporting.Result as Result
import qualified Reporting.Warning as Warning


patternMatches
    :: Module.Interfaces
    -> Module.CanonicalModule
    -> Result.Result Warning.Warning e ()
patternMatches interfaces modul =
  let
    name = Module.names modul
    body = Module.body modul
  in
    checkExpression
      (toTagDict interfaces name (Module.datatypes body))
      (Module.program body)


-- TAG DICT

type TagDict =
  Map.Map Var.Home (Map.Map Tag [(Tag, Int)])


type Tag = String


toTagDict :: Module.Interfaces -> Module.Name -> Module.ADTs -> TagDict
toTagDict interfaces localName localAdts =
  let
    listTags =
        [ ("::", 2), ("[]", 0) ]

    builtinDict =
        Map.singleton Var.BuiltIn $
          Map.fromList
            [ ("::", listTags)
            , ("[]", listTags)
            ]

    interfaceDict =
        interfaces
          |> Map.map (toTagMapping . Module.iAdts)
          |> Map.mapKeysMonotonic Var.Module
          |> Map.insert (Var.Module localName) (toTagMapping localAdts)
  in
    Map.union builtinDict interfaceDict


toTagMapping :: Module.ADTs -> Map.Map Tag ([(Tag,Int)])
toTagMapping adts =
  let
    toTagAndArity (_tvars, tagInfoList) =
        let
          info = map (\(tag, args) -> (tag, length args)) tagInfoList
        in
          map (second (const info)) tagInfoList
  in
    Map.elems adts
      |> concatMap toTagAndArity
      |> Map.fromList


lookupOtherTags :: Var.Canonical -> TagDict -> [(Tag, Int)]
lookupOtherTags (Var.Canonical home name) tagDict =
  case Map.lookup name =<< Map.lookup home tagDict of
    Just otherTags ->
        otherTags

    Nothing ->
      if Help.isTuple name then
        [(name, read (drop 6 name))]
      else
        error
          "Since the Nitpick phase happens after canonicalization and type \
          \inference, it is impossible that a pattern in a case cannot be \
          \found."


-- CHECK EXPRESSIONS

checkExpression
    :: TagDict
    -> Canonical.Expr
    -> Result.Result Warning.Warning e ()
checkExpression tagDict (A.A region expression) =
  let
    go =
      checkExpression tagDict

    go2 a b =
      go a <* go b

    goPattern =
      checkPatterns tagDict region
  in
  case expression of
    E.Literal _ ->
        Result.ok ()

    E.Var _ ->
        Result.ok ()

    E.Range low high ->
        go2 low high

    E.ExplicitList listExprs ->
        F.traverse_ go listExprs

    E.Binop _ leftExpr rightExpr ->
        go2 leftExpr rightExpr

    E.Lambda pattern body ->
        goPattern [pattern]
        <* go body

    E.App func arg ->
        go2 func arg

    E.MultiIf branches finally ->
        F.traverse_ (uncurry go2) branches
        <* go finally

    E.Let defs body ->
        go body
          <* F.traverse_ goDef defs
      where
        goDef (Canonical.Definition pattern expr _) =
            goPattern [pattern]
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

    E.Port impl ->
        case impl of
          E.In _ _ ->
              Result.ok ()

          E.Out _ expr _ ->
              go expr

          E.Task _ expr _ ->
              go expr

    E.GLShader _ _ _ ->
        Result.ok ()

    E.Crash _ ->
        Result.ok ()


-- CHECK PATTERNS

checkPatterns
    :: TagDict
    -> Region.Region
    -> [Pattern.CanonicalPattern]
    -> Result.Result Warning.Warning e ()
checkPatterns tagDict region patterns =
  checkPatternsHelp tagDict region [Anything] patterns


checkPatternsHelp
    :: TagDict
    -> Region.Region
    -> [Pattern]
    -> [Pattern.CanonicalPattern]
    -> Result.Result Warning.Warning e ()
checkPatternsHelp tagDict region unhandled patterns =
  case (unhandled, patterns) of
    ([], []) ->
        return ()

    (_:_, []) ->
        Result.warn region (Warning.InexhaustivePatternMatch unhandled)

    (_, pattern@(A.A localRegion _) : remainingPatterns) ->
        do  newUnhandled <- filterPatterns tagDict localRegion pattern unhandled
            checkPatternsHelp tagDict region newUnhandled remainingPatterns


filterPatterns
    :: TagDict
    -> Region.Region
    -> Pattern.CanonicalPattern
    -> [Pattern]
    -> Result.Result Warning.Warning e [Pattern]
filterPatterns tagDict region pattern unhandled =
  let
    nitPattern =
      fromCanonicalPattern pattern

    noIntersection pat =
      intersection pat nitPattern == Nothing
  in
    if all noIntersection unhandled then
      do  Result.warn region Warning.RedundantPatternMatch
          return unhandled
    else
      do  let complementPatterns = complement tagDict nitPattern
          return $
            concatMap
              (\p -> Maybe.mapMaybe (intersection p) complementPatterns)
              unhandled


-- PATTERN INTERSECTION

intersection :: Pattern -> Pattern -> Maybe Pattern
intersection pattern1 pattern2 =
  case (pattern1, pattern2) of
    (Alias _ pattern1', _) ->
        intersection pattern1' pattern2

    (_, Alias _ pattern2') ->
        intersection pattern1 pattern2'

    (Anything, _) ->
        Just pattern2

    (Var _, _) ->
        Just pattern2

    (_, Anything) ->
        Just pattern1

    (_, Var _) ->
        Just pattern1

    (Data ctor1 args1, Data ctor2 args2) ->
        if ctor1 /= ctor2 then
          Nothing
        else
          Data ctor1 <$> sequence (zipWith intersection args1 args2)

    (Record _, Record _) ->
        Just pattern1

    (Literal lit1, Literal lit2) ->
        if lit1 == lit2 then
          Just pattern1
        else
          Nothing

    (AnythingBut literals, Literal lit) ->
        if Set.member lit literals then
          Nothing
        else
          Just pattern2

    (Literal lit, AnythingBut literals) ->
        if Set.member lit literals then
          Nothing
        else
          Just pattern1

    (AnythingBut literals1, AnythingBut literals2) ->
        Just (AnythingBut (Set.union literals1 literals2))

    _ ->
        Nothing


-- PATTERN COMPLEMENT

complement :: TagDict -> Pattern -> [Pattern]
complement tagDict nitPattern =
  case nitPattern of
    Record _fields ->
        []

    Alias _name pattern ->
        complement tagDict pattern

    Var _name ->
        []

    Anything ->
        []

    Literal lit ->
        [AnythingBut (Set.singleton lit)]

    AnythingBut literals ->
        map Literal (Set.toList literals)

    Data ctor patterns ->
        complementData tagDict ctor patterns


complementData :: TagDict -> Var.Canonical -> [Pattern] -> [Pattern]
complementData tagDict tag patterns =
  let
    otherTags =
        lookupOtherTags tag tagDict

    tagComplements =
        Maybe.mapMaybe (tagToPattern tag) otherTags

    arity =
        length patterns

    argComplements =
        concat (zipWith (makeArgComplements tagDict tag arity) [0..] patterns)
  in
    tagComplements ++ argComplements


tagToPattern :: Var.Canonical -> (Tag, Int) -> Maybe Pattern
tagToPattern (Var.Canonical home rootTag) (tag, arity) =
  if rootTag == tag then
    Nothing
  else
    Just (Data (Var.Canonical home tag) (replicate arity Anything))


makeArgComplements :: TagDict -> Var.Canonical -> Int -> Int -> Pattern -> [Pattern]
makeArgComplements tagDict tag arity index argPattern =
  let
    complementList =
        complement tagDict argPattern

    padArgs pattern =
        replicate index Anything
        ++ pattern
        : replicate (arity - index - 1) Anything
  in
    map (Data tag . padArgs) complementList

