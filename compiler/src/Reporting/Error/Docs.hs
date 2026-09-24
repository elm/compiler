{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Docs
  ( Error(..)
  , SyntaxProblem(..)
  , NameProblem(..)
  , DefProblem(..)
  , toReports
  )
  where


import qualified Data.NonEmptyList as NE

import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import Parse.Primitives (Cursor)
import Parse.Symbol (BadOperator(..))
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Render.Code as Code
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Report as Report



data Error
  = NoDocs A.Region
  | ImplicitExposing A.Region
  | SyntaxProblem SyntaxProblem
  | NameProblems (NE.List NameProblem)
  | DefProblems (NE.List DefProblem)


data SyntaxProblem
  = Op Cursor
  | OpBad BadOperator Cursor
  | Name Cursor
  | Space E.Space Cursor
  | Comma Cursor
  | BadEnd Cursor


data NameProblem
  = NameDuplicate_Type T.Name A.Region A.Region
  | NameDuplicate_Var N.Name A.Region A.Region
  | NameDuplicate_Op Op.Name A.Region A.Region
  | NameOnlyInDocs_Type T.Name A.Region
  | NameOnlyInDocs_Var N.Name A.Region
  | NameOnlyInDocs_Op Op.Name A.Region
  | NameOnlyInExports_Type T.Name A.Region
  | NameOnlyInExports_Var N.Name A.Region
  | NameOnlyInExports_Op Op.Name A.Region


data DefProblem
  = NoComment_Type T.Name A.Region
  | NoComment_Var N.Name A.Region
  | NoAnnotation_Var N.Name A.Region
  | NoAnnotation_Op Op.Name A.Region



-- TO REPORTS


toReports :: Code.Source -> Error -> NE.List Report.Report
toReports source err =
  case err of
    NoDocs region ->
      NE.singleton $
      Report.Report "NO DOCS" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "You must have a documentation comment between the module\
              \ declaration and the imports."
          ,
            D.reflow
              "Learn more at <https://package.elm-lang.org/help/documentation-format>"
          )

    ImplicitExposing region ->
      NE.singleton $
      Report.Report "IMPLICIT EXPOSING" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I need you to be explicit about what this module exposes:"
          ,
            D.reflow $
              "A great API usually hides some implementation details, so it is rare that\
              \ everything in the file should be exposed. And requiring package authors\
              \ to be explicit about this is a way of adding another quality check before\
              \ code gets published. So as you write out the public API, ask yourself if\
              \ it will be easy to understand as people read the documentation!"
          )

    SyntaxProblem problem ->
      NE.singleton $
        toSyntaxProblemReport source problem

    NameProblems problems ->
      fmap (toNameProblemReport source) problems

    DefProblems problems ->
      fmap (toDefProblemReport source) problems



-- SYNTAX PROBLEM


toSyntaxProblemReport :: Code.Source -> SyntaxProblem -> Report.Report
toSyntaxProblemReport source problem =
  let
    toSyntaxReport cur details =
      let
        region = toRegion cur
      in
      Report.Report "PROBLEM IN DOCS" region [] $
        Code.toSnippet source region Nothing
          ( D.reflow "I was partway through parsing your module documentation, but I got stuck here:"
          , D.stack $
              [ D.reflow details
              , D.toSimpleHint $
                  "Read through <https://package.elm-lang.org/help/documentation-format> for\
                  \ tips on how to write module documentation!"
              ]
          )
  in
  case problem of
    Op cur ->
      toSyntaxReport cur $
        "I am trying to parse an operator like (+) or (*) but something is going wrong."

    OpBad _ cur ->
      toSyntaxReport cur $
        "I am trying to parse an operator like (+) or (*) but it looks like you are using\
        \ a reserved symbol in this case."

    Name cur ->
      toSyntaxReport cur $
        "I was expecting to see the name of another exposed value from this module."

    Space space cur ->
      E.toSpaceReport source space cur

    Comma cur ->
      toSyntaxReport cur $
        "I was expecting to see a comma next."

    BadEnd cur ->
      toSyntaxReport cur $
        "I am not really sure what I am getting stuck on though."


toRegion :: Cursor -> A.Region
toRegion cur =
  A.Region cur cur



-- NAME PROBLEM


toNameProblemReport :: Code.Source -> NameProblem -> Report.Report
toNameProblemReport source problem =
  case problem of
    NameDuplicate_Type n r1 r2 -> nameDuplicate n r1 r2 T.nameToChars
    NameDuplicate_Var  n r1 r2 -> nameDuplicate n r1 r2 N.toChars
    NameDuplicate_Op   n r1 r2 -> nameDuplicate n r1 r2 Op.toChars
    NameOnlyInDocs_Type    n r -> nameOnlyInDocs n r T.nameToChars
    NameOnlyInDocs_Var     n r -> nameOnlyInDocs n r N.toChars
    NameOnlyInDocs_Op      n r -> nameOnlyInDocs n r Op.toChars
    NameOnlyInExports_Type n r -> nameOnlyInExports n r T.nameToChars
    NameOnlyInExports_Var  n r -> nameOnlyInExports n r N.toChars
    NameOnlyInExports_Op   n r -> nameOnlyInExports n r Op.toChars
  where
    nameDuplicate name r1 r2 toChars =
      Report.Report "DUPLICATE DOCS" r2 [] $
        Code.toPair source r1 r2
          (
            D.reflow $
              "There can only be one `" <> toChars name
              <> "` in your module documentation, but it is listed twice:"
          ,
            "Remove one of them!"
          )
          (
            D.reflow $
              "There can only be one `" <> toChars name
              <> "` in your module documentation, but I see two. One here:"
          ,
            "And another one over here:"
          ,
            "Remove one of them!"
          )

    nameOnlyInDocs name region toChars =
      Report.Report "DOCS MISTAKE" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I do not see `" <> toChars name
              <> "` in the `exposing` list, but it is in your module documentation:"
          ,
            D.reflow $
              "Does it need to be added to the `exposing` list as well? Or maybe you removed `"
              <> toChars name <> "` and forgot to delete it here?"
          )

    nameOnlyInExports name region toChars =
      Report.Report "DOCS MISTAKE" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "I do not see `" <> toChars name
              <> "` in your module documentation, but it is in your `exposing` list:"
          ,
            D.stack
              [ D.reflow $
                  "Add a line like `@docs " <> toChars name
                  <> "` to your module documentation!"
              , D.link "Note" "See" "docs" "for more guidance on writing high quality docs."
              ]
          )



-- DEF PROBLEM


toDefProblemReport :: Code.Source -> DefProblem -> Report.Report
toDefProblemReport source problem =
  case problem of
    NoComment_Type   n r -> noComment    n r T.nameToChars
    NoComment_Var    n r -> noComment    n r N.toChars
    NoAnnotation_Var n r -> noAnnotation n r N.toChars
    NoAnnotation_Op  n r -> noAnnotation n r Op.toChars
  where
    noComment name region toChars =
      Report.Report "NO DOCS" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "The `" <> toChars name <> "` definition does not have a documentation comment."
          ,
            D.stack
              [ D.reflow $
                  "Add documentation with nice examples of how to use it!"
              , D.link "Note" "Read" "docs" "for more advice on writing great docs. There are a couple important tricks!"
              ]
          )

    noAnnotation name region toChars =
      Report.Report "NO TYPE ANNOTATION" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "The `" <> toChars name <> "` definition does not have a type annotation."
          ,
            D.stack
              [ D.reflow $
                  "I use the type variable names from your annotations when generating docs. So if\
                  \ you say `Html msg` in your type annotation, I can use `msg` in the docs and make\
                  \ them a bit clearer. So add an annotation and try to use nice type variables!"
              , D.link "Note" "Read" "docs" "for more advice on writing great docs. There are a couple important tricks!"
              ]
          )

