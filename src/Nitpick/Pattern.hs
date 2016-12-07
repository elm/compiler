{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Nitpick.Pattern
  ( Pattern(..)
  , fromCanonicalPattern
  , toText
  )
  where

import qualified Data.List as List
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)

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
    P.Ctor name patterns ->
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


toText :: Pattern -> Text
toText pattern =
  LazyText.toStrict (toLazyText (toBuilder False pattern))


toBuilder :: Bool -> Pattern -> Builder
toBuilder needsParens pattern =
  case pattern of
    Ctor tag [first,rest] | Var.toText tag == "::" ->
        toBuilder (isCons first) first <> " :: " <> toBuilder False rest

    Ctor tag args ->
        if Var.isTuple tag then
            if null args then "()" else
              "( "
              <> mconcat (List.intersperse ", " (map (toBuilder False) args))
              <> " )"

        else
          addParensIf (needsParens && not (null args)) $
            mconcat (List.intersperse " " (fromText (Var.toText tag) : map (toBuilder True) args))

    Anything ->
        "_"

    Literal literal ->
        L.toBuilder literal


addParensIf :: Bool -> Builder -> Builder
addParensIf needsParens str =
  if needsParens then
      "(" <> str <> ")"

  else
      str


isCons :: Pattern -> Bool
isCons pattern =
  case pattern of
    Ctor tag [_,_] ->
        Var.toText tag == "::"

    _ ->
        False
