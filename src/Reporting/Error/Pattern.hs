{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Pattern where

import qualified Nitpick.Pattern as Pattern
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Report as Report


data Error
    = Incomplete [Pattern.Pattern]
    | Redundant


-- TO REPORT

toReport :: P.Dealiaser -> Error -> Report.Report
toReport dealiaser err =
  case err of
    Incomplete unhandled ->
        Report.simple
          "MISSING PATTERN"
          "The following `case` does not handle all possible inputs."
          ( "The following patterns are not being handled:\n"
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


incompleteExplanation :: String
incompleteExplanation =
  unlines
    [ "Did you add a new tag to a union type? Add how it should be handled here and"
    , "you will be all set!"
    , ""
    , "If you want to go quick and implement this later, you can end the pattern with:"
    , ""
    , "    _ ->"
    , "        Debug.crash \"TODO\""
    , ""
    , "When I see cases that end with this pattern, I augment the runtime error with"
    , "the module name, the relevant line numbers, and the value led down that branch."
    , ""
    , "That said, you almost never want this in production code. If you think there"
    , "are certain cases that are impossible in practice, consider refining how you"
    , "model the data. If it can never happen in practice, you can probably define a"
    , "union type such that the \"impossible\" cannot even be constructed in the"
    , "first place. That is how you make something \"impossible\" truly impossible."
    , ""
    , "I won't say \"I told you so\" in the runtime error, but I'll be thinking it ;)"
    ]
