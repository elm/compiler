module Nitpick.Pattern where

import qualified Data.List as List
import qualified Data.Set as Set

import qualified AST.Literal as L
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A


data Pattern
    = Data Var.Canonical [Pattern]
    | Record [String]
    | Alias String Pattern
    | Var String
    | Anything
    | Literal L.Literal
    | AnythingBut (Set.Set L.Literal)
    deriving (Eq, Ord)


fromCanonicalPattern :: P.Canonical -> Pattern
fromCanonicalPattern (A.A _ pattern) =
  case pattern of
    P.Data tag patterns ->
        Data tag (map fromCanonicalPattern patterns)

    P.Record fields ->
        Record fields

    P.Alias name pattern ->
        Alias name (fromCanonicalPattern pattern)

    P.Var name ->
        Var name

    P.Anything ->
        Anything

    P.Literal (L.Boolean True) ->
        Data (Var.builtin "True") []

    P.Literal (L.Boolean False) ->
        Data (Var.builtin "False") []

    P.Literal literal ->
        Literal literal


-- TO STRING

toString :: Bool -> Pattern -> String
toString needsParens pattern =
  case pattern of
    Data tag [first,rest] | Var.toString tag == "::" ->
        toString (isCons first) first
        ++ " :: "
        ++ toString False rest

    Data tag args ->
        if Var.isTuple tag then
            "(" ++ List.intercalate ", " (map (toString False) args) ++ ")"

        else
          addParensIf (needsParens && not (null args)) $
            List.intercalate " " (Var.toString tag : map (toString True) args)

    Record fields ->
        "{ " ++ List.intercalate ", " fields ++ " }"

    Alias alias realPattern ->
        addParensIf needsParens $
          toString False realPattern ++ " as " ++ alias

    Var name ->
        name

    Anything ->
        "_"

    Literal literal ->
        L.toString literal

    AnythingBut literalSet ->
        case map L.toString (Set.toList literalSet) of
          [] ->
              error "should not have an AnythingBut with no literals"

          [x] ->
              "<values besides " ++ x ++ ">"

          [x,y] ->
              "<values besides " ++ x ++ " and " ++ y ++ ">"

          literals ->
              "<values besides "
              ++ List.intercalate ", " (init literals)
              ++ ", and " ++ last literals ++ ">"


addParensIf :: Bool -> String -> String
addParensIf needsParens str =
  if needsParens then
      "(" ++ str ++ ")"

  else
      str


isCons :: Pattern -> Bool
isCons pattern =
  case pattern of
    Data tag [_,_] ->
        Var.toString tag == "::"

    _ ->
        False
