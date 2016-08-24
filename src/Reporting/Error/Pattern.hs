{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Pattern where

import qualified Data.Set as Set
import Text.PrettyPrint.ANSI.Leijen (text)

import qualified Nitpick.Pattern as Pattern
import qualified Reporting.Error.Helpers as Help
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Report as Report



data Error
    = Incomplete Origin (Set.Set Pattern.Pattern)
    | Redundant


data Origin
    = Arg
    | LetBound
    | Case



-- TO REPORT


toReport :: RenderType.Localizer -> Error -> Report.Report
toReport _localizer err =
  case err of
    Incomplete Arg unhandled ->
        Report.report
          "PARTIAL PATTERN"
          Nothing
          "This pattern does not cover all possible inputs."
          (text (unhandledError unhandled toCaseMessage))

    Incomplete LetBound unhandled ->
        Report.report
          "PARTIAL PATTERN"
          Nothing
          "The pattern used here does not cover all possible values."
          (text (unhandledError unhandled toCaseMessage))

    Incomplete Case unhandled ->
        Report.report
          "MISSING PATTERNS"
          Nothing
          "This `case` does not have branches for all possibilities."
          (text (unhandledError unhandled (missingBranchesMessage (length unhandled))))

    Redundant ->
        Report.report
          "REDUNDANT PATTERN"
          Nothing
          "The following pattern is redundant. Remove it."
          (text "Any value with this shape will be handled by a previous pattern.")


unhandledError :: Set.Set Pattern.Pattern -> String -> String
unhandledError unhandledPatterns relevantMessage =
  let
    (visiblePatterns, rest) =
        splitAt 4 (Set.toList unhandledPatterns)

    patternList =
        map (Pattern.toString False) visiblePatterns
        ++ if null rest then [] else ["..."]
  in
    "You need to account for the following values:\n"
    ++ concatMap ("\n    " ++) patternList ++ "\n"
    ++ "\n"
    ++ relevantMessage ++ "\n"
    ++ "\n"
    ++ "If you are seeing this error for the first time, check out these hints:\n"
    ++ Help.hintLink "missing-patterns" ++ "\n"
    ++ "The recommendations about wildcard patterns and `Debug.crash` are important!"


toCaseMessage :: String
toCaseMessage =
  "Switch to a `case` expression to handle all possible patterns."


missingBranchesMessage :: Int -> String
missingBranchesMessage numUnhandled =
  if numUnhandled == 1 then
    "Add a branch to cover this pattern!"

  else
    "Add branches to cover each of these patterns!"

