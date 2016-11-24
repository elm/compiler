{-# OPTIONS_GHC -Wall #-}
module Nitpick.Pattern
  ( Pattern(..)
  , fromCanonicalPattern
  , toString
  )
  where

import qualified Data.List as List

import qualified AST.Literal as L
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A



-- PATTERN


data Pattern
    = Ctor Var.Canonical [Pattern]
    | Anything
    | Literal L.Literal
    deriving (Eq)


fromCanonicalPattern :: P.Canonical -> Pattern
fromCanonicalPattern (A.A _ pattern) =
  case pattern of
    P.Data name patterns ->
        Ctor name (map fromCanonicalPattern patterns)

    P.Record _ ->
        Anything

    P.Alias _ subPattern ->
        fromCanonicalPattern subPattern

    P.Var _ ->
        Anything

    P.Anything ->
        Anything

    P.Literal (L.Boolean True) ->
        Ctor (Var.builtin "True") []

    P.Literal (L.Boolean False) ->
        Ctor (Var.builtin "False") []

    P.Literal literal ->
        Literal literal



-- TO STRING


toString :: Bool -> Pattern -> String
toString needsParens pattern =
  case pattern of
    Ctor tag [first,rest] | Var.toString tag == "::" ->
        toString (isCons first) first
        ++ " :: "
        ++ toString False rest

    Ctor tag args ->
        if Var.isTuple tag then
            if null args then "()" else
              "( " ++ List.intercalate ", " (map (toString False) args) ++ " )"

        else
          addParensIf (needsParens && not (null args)) $
            List.intercalate " " (Var.toString tag : map (toString True) args)

    Anything ->
        "_"

    Literal literal ->
        L.toString literal


addParensIf :: Bool -> String -> String
addParensIf needsParens str =
  if needsParens then
      "(" ++ str ++ ")"

  else
      str


isCons :: Pattern -> Bool
isCons pattern =
  case pattern of
    Ctor tag [_,_] ->
        Var.toString tag == "::"

    _ ->
        False
