{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Pattern where

import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Nitpick.Pattern as Pattern
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Report as Report


data Error
    = Incomplete Origin [Pattern.Pattern]
    | Redundant


data Origin
    = Arg
    | LetBound
    | Case


-- TO REPORT

toReport :: P.Dealiaser -> Error -> Report.Report
toReport dealiaser err =
  case err of
    Incomplete Arg unhandled ->
        Report.simple
          "PARTIAL PATTERN"
          "This argument does not cover all possible inputs."
          ( "Any of these patterns can cause a crash:\n"
            ++ viewPatternList unhandled
            ++ "\n\n"
            ++ partialExplanation unhandled
          )

    Incomplete LetBound unhandled ->
        Report.simple
          "PARTIAL PATTERN"
          "The pattern used here does not cover all possible values."
          ( "Any of these patterns can cause a crash:\n"
            ++ viewPatternList unhandled
            ++ "\n\n"
            ++ partialExplanation unhandled
          )

    Incomplete Case unhandled ->
        Report.simple
          "INCOMPLETE PATTERN MATCH"
          "This `case` does not have branches for all possibilities."
          ( "Any of these patterns can cause a crash:\n"
            ++ viewPatternList unhandled
            ++ "\n\n"
            ++ incompleteExplanation
          )

    Redundant ->
        Report.simple
          "REDUNDANT PATTERN"
          "The following pattern is redundant."
          "Any value with this shape will be handled by a previous pattern."


viewPatternList :: [Pattern.Pattern] -> String
viewPatternList unhandledPatterns =
  let
    (showPatterns, rest) =
      splitAt 4 unhandledPatterns
  in
    concatMap ((++) "\n    ") $
      map (Pattern.toString False) showPatterns
      ++ if null rest then [] else ["..."]


partialExplanation :: [Pattern.Pattern] -> String
partialExplanation unhandledPatterns =

  "You probably want to switch to a `case` expression to handle all possible\n"
  ++ "patterns in a simple way. If you think some cases are impossible, you can use\n"
  ++ "`Debug.crash` to make that thinking explicit in your code.\n"
  ++ "<http://package.elm-lang.org/packages/elm-lang/core/latest/Debug#crash>"
  ++

  if isNothing unhandledPatterns then

      "\n\nSometimes you *know* a value will never be Nothing, though such cases are very\n"
      ++ "rare. If you are certain you are in that case and that there is no better way\n"
      ++ "to do things, I'd recommend creating a function `unsafe` that makes the danger\n"
      ++ "explicit in the code. <https://github.com/elm-lang/core/issues/215>"

  else if isEmptyList unhandledPatterns then

      "\n\nSometimes you *know* a list cannot be empty, though such cases are relatively\n"
      ++ "rare. If you are certain you are in that case and that there is no better\n"
      ++ "way to do things, I'd recommend creating a function like `unsafe` that makes\n"
      ++ "the danger explicit in the code. <https://github.com/elm-lang/core/issues/215>"

  else

      ""


isNothing :: [Pattern.Pattern] -> Bool
isNothing unhandledPatterns =
  case unhandledPatterns of
    [Pattern.Data (Var.Canonical (Var.Module moduleName) "Nothing") []] ->
        moduleName == ModuleName.inCore ["Maybe"]

    _ ->
        False


isEmptyList :: [Pattern.Pattern] -> Bool
isEmptyList unhandledPatterns =
  case unhandledPatterns of
    [Pattern.Data (Var.Canonical Var.BuiltIn "[]") []] ->
        True

    _ ->
        False


incompleteExplanation :: String
incompleteExplanation =
  "Did you add a new tag to a union type? Add how it should be handled here and\n"
  ++ "you will be all set!\n"
  ++ "\n"
  ++ "If you want to go quick and implement this later, you may want to look into\n"
  ++ "using `Debug.crash` which has some cool error reporting abilities.\n"
  ++ "<http://package.elm-lang.org/packages/elm-lang/core/latest/Debug#crash>"

