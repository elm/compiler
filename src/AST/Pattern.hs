{-# OPTIONS_GHC -Wall #-}
module AST.Pattern where

import qualified Data.List as List
import qualified Data.Set as Set

import qualified AST.Literal as L
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


type Pattern ann var =
    A.Annotated ann (Pattern' ann var)


data Pattern' ann var
    = Data var [Pattern ann var]
    | Record [String]
    | Alias String (Pattern ann var)
    | Var String
    | Anything
    | Literal L.Literal


type Raw =
    Pattern R.Region Var.Raw


type Raw' =
    Pattern' R.Region Var.Raw


type Canonical =
    Pattern R.Region Var.Canonical


isVar :: String -> Pattern ann var -> Bool
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
        A.at end end (Data (Var.Raw "[]") [])

    pattern@(A.A (R.Region start _) _) : rest ->
        A.at start end (Data (Var.Raw "::") [pattern, list end rest])


consMany :: R.Position -> [Raw] -> Raw
consMany end patterns =
  let cons hd@(A.A (R.Region start _) _) tl =
          A.at start end (Data (Var.Raw "::") [hd, tl])
  in
      foldr1 cons patterns


tuple :: [Raw] -> Raw'
tuple patterns =
  Data (Var.Raw ("_Tuple" ++ show (length patterns))) patterns



-- FIND VARIABLES


boundVars :: Pattern ann var -> [A.Annotated ann String]
boundVars (A.A ann pattern) =
  case pattern of
    Var x ->
        [A.A ann x]

    Alias name realPattern ->
        A.A ann name : boundVars realPattern

    Data _ patterns ->
        concatMap boundVars patterns

    Record fields ->
        map (A.A ann) fields

    Anything ->
        []

    Literal _ ->
        []


member :: String -> Pattern ann var -> Bool
member name pattern =
  any (name==) (map A.drop (boundVars pattern))


boundVarSet :: Pattern ann var -> Set.Set String
boundVarSet pattern =
  Set.fromList (map A.drop (boundVars pattern))


boundVarList :: Pattern ann var -> [String]
boundVarList pattern =
  Set.toList (boundVarSet pattern)



-- TO STRING


toString :: Bool -> Canonical -> String
toString needsParens (A.A _ pattern) =
  case pattern of
    Var name ->
      name

    Data name [] ->
      if Var.isTuple name then
        "()"
      else
        Var.toString name

    Data name args ->
      if Var.isTuple name then
        "( " ++ List.intercalate ", " (map (toString False) args) ++ " )"
      else
        parensIf needsParens $
          Var.toString name ++ concatMap ((" "++) . toString True) args

    Record fields ->
      "{" ++ List.intercalate "," fields ++ "}"

    Alias alias subPattern ->
      parensIf needsParens $
        toString False subPattern ++ " as " ++ alias

    Anything ->
      "_"

    Literal literal ->
      L.toString literal


parensIf :: Bool -> String -> String
parensIf needsParens str =
  if needsParens then "(" ++ str ++ ")" else str
