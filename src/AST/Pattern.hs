{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Pattern where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Literal as L
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


type Pattern ann var =
    A.Annotated ann (Pattern' ann var)


data Pattern' ann var
    = Ctor var [Pattern ann var]
    | Record [Text]
    | Alias Text (Pattern ann var)
    | Var Text
    | Anything
    | Literal L.Literal


type Raw =
    Pattern R.Region Var.Raw


type Raw' =
    Pattern' R.Region Var.Raw


type Canonical =
    Pattern R.Region Var.Canonical


isVar :: Text -> Pattern ann var -> Bool
isVar name (A.A _ pattern) =
  case pattern of
    Var pName ->
      name == pName

    _ ->
      False


list :: R.Position -> [Raw] -> Raw
list end patterns =
  case patterns of
    [] ->
        A.at end end (Ctor (Var.Raw "[]") [])

    pattern@(A.A (R.Region start _) _) : rest ->
        A.at start end (Ctor (Var.Raw "::") [pattern, list end rest])


tuple :: [Raw] -> Raw'
tuple patterns =
  let
    name =
      Text.append "_Tuple" (Text.pack (show (length patterns)))
  in
    Ctor (Var.Raw name) patterns



-- FIND VARIABLES


boundVars :: Pattern ann var -> [A.Annotated ann Text]
boundVars (A.A ann pattern) =
  case pattern of
    Var x ->
        [A.A ann x]

    Alias name realPattern ->
        A.A ann name : boundVars realPattern

    Ctor _ patterns ->
        concatMap boundVars patterns

    Record fields ->
        map (A.A ann) fields

    Anything ->
        []

    Literal _ ->
        []


member :: Text -> Pattern ann var -> Bool
member name pattern =
  elem name (map A.drop (boundVars pattern))


boundVarSet :: Pattern ann var -> Set.Set Text
boundVarSet pattern =
  Set.fromList (map A.drop (boundVars pattern))


boundVarList :: Pattern ann var -> [Text]
boundVarList pattern =
  Set.toList (boundVarSet pattern)



-- TO STRING


toString :: Bool -> Canonical -> String
toString needsParens (A.A _ pattern) =
  case pattern of
    Var name ->
      Text.unpack name

    Ctor name [] ->
      if Var.isTuple name then
        "()"
      else
        Var.toString name

    Ctor name args ->
      if Var.isTuple name then
        "( " ++ List.intercalate ", " (map (toString False) args) ++ " )"
      else
        parensIf needsParens $
          Var.toString name ++ concatMap ((" "++) . toString True) args

    Record fields ->
      "{" ++ List.intercalate "," (map Text.unpack fields) ++ "}"

    Alias alias subPattern ->
      parensIf needsParens $
        toString False subPattern ++ " as " ++ Text.unpack alias

    Anything ->
      "_"

    Literal literal ->
      L.toString literal


parensIf :: Bool -> String -> String
parensIf needsParens str =
  if needsParens then "(" ++ str ++ ")" else str
