{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Pattern where

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Nitpick.Pattern as Pattern
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Region as Region
import qualified Reporting.Report as Report
import qualified Reporting.Helpers as Help
import Reporting.Helpers ((<>), text)



data Error
    = Incomplete Origin [Pattern.Pattern]
    | Redundant Region.Region Int


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

    Redundant region index ->
        Report.report
          "REDUNDANT PATTERN"
          (Just region)
          ("The " <> Help.ordinalize index <> " pattern is redundant. Remove it.")
          (text "Any value with this shape will be handled by a previous pattern.")


unhandledError :: [Pattern.Pattern] -> Text -> Text
unhandledError unhandledPatterns relevantMessage =
  let
    (visiblePatterns, rest) =
      splitAt 4 unhandledPatterns

    patternList =
        map Pattern.toText visiblePatterns
        ++ if null rest then [] else ["..."]

    noun =
      if length unhandledPatterns == 1 then "pattern" else "patterns"
  in
    "You need to account for the following " <> noun <> ":\n"
    <> Text.concat (map ("\n    " <>) patternList) <> "\n"
    <> "\n"
    <> relevantMessage <> "\n"
    <> "\n"
    <> "If you are seeing this error for the first time, check out these hints:\n"
    <> Help.hintLink "missing-patterns" <> "\n"
    <> "The recommendations about wildcard patterns and `Debug.crash` are important!"


toCaseMessage :: Text
toCaseMessage =
  "Switch to a `case` expression to handle all possible patterns."


missingBranchesMessage :: Int -> Text
missingBranchesMessage numUnhandled =
  if numUnhandled == 1 then
    "Add a branch to cover this pattern."

  else
    "Add branches to cover each of these patterns."

