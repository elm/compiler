{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Pattern where

import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)

import qualified AST.Helpers as Help
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
      Help.makeTuple (length patterns)
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



-- TO BUILDER


toString :: Canonical -> String
toString var =
  LazyText.unpack (toLazyText (toBuilder False var))


toText :: Canonical -> Text
toText var =
  LazyText.toStrict (toLazyText (toBuilder False var))


toBuilder :: Bool -> Canonical -> Builder
toBuilder needsParens (A.A _ pattern) =
  case pattern of
    Var name ->
      fromText name

    Ctor name [] ->
      if Var.isTuple name then "()" else fromText (Var.toText name)

    Ctor name args ->
      if Var.isTuple name then
        "( " <> mconcat (List.intersperse ", " (map (toBuilder False) args)) <> " )"
      else
        parensIf needsParens $
          fromText (Var.toText name) <> mconcat (map (\arg -> " " <> toBuilder True arg) args)

    Record fields ->
      "{" <> mconcat (List.intersperse "," (map fromText fields)) <> "}"

    Alias alias subPattern ->
      parensIf needsParens $
        toBuilder False subPattern <> " as " <> fromText alias

    Anything ->
      "_"

    Literal literal ->
      L.toBuilder literal


parensIf :: Bool -> Builder -> Builder
parensIf needsParens builder =
  if needsParens then "(" <> builder <> ")" else builder
