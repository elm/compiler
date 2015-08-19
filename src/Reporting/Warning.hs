{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Warning where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Text.PrettyPrint as P
import Text.PrettyPrint ((<+>))

import qualified AST.Module as Module
import qualified AST.Type as Type
import qualified Nitpick.Pattern as Pattern
import qualified Reporting.Annotation as A
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Report as Report


-- ALL POSSIBLE WARNINGS

data Warning
    = UnusedImport Module.Name
    | MissingTypeAnnotation String Type.Canonical
    | InexhaustivePatternMatch [Pattern.Pattern]
    | RedundantPatternMatch


-- TO STRING

toString :: P.Dealiaser -> String -> String -> A.Located Warning -> String
toString dealiaser location source (A.A region warning) =
    Report.toString location region (toReport dealiaser warning) source


print :: P.Dealiaser -> String -> String -> A.Located Warning -> IO ()
print dealiaser location source (A.A region warning) =
    Report.printWarning location region (toReport dealiaser warning) source


toReport :: P.Dealiaser -> Warning -> Report.Report
toReport dealiaser warning =
  case warning of
    UnusedImport moduleName ->
        Report.simple
          "unused import"
          ("Module `" ++ Module.nameToString moduleName ++ "` is unused.")
          ""

    MissingTypeAnnotation name inferredType ->
        Report.simple
          "missing type annotation"
          ("Top-level value `" ++ name ++ "` does not have a type annotation.")
          ( "The type annotation you want looks something like this:\n\n"
            ++ P.render (P.nest 4 typeDoc)
          )
      where
        typeDoc =
          P.hang
            (P.text name <+> P.colon)
            4
            (P.pretty dealiaser False inferredType)

    InexhaustivePatternMatch unhandled ->
        Report.simple
          "missing pattern"
          "The following case expression does not handle all possible inputs."
          ( "The following patterns are not being handled:\n"
            ++ viewPatternList unhandled
            ++ "\n\n"
            ++ "If we get values like this, we have no choice but to crash! Normally this means\n"
            ++ "you just added a new tag to a union type, but sometimes it can be because there\n"
            ++ "is something very odd about the data you are modeling. In those rare cases, you\n"
            ++ "probably want to either:\n"
            ++ "\n"
            ++ "  1. Rethink the model. Maybe some cases are \"impossible\" but it is still\n"
            ++ "     possible to construct such cases. Is there a way to model the values more\n"
            ++ "     precisely such that \"impossible\" values truly are impossible?\n"
            ++ "  2. End the pattern match with a wildcard match that leads to an expression\n"
            ++ "     like: Debug.crash \"If you are reading this, I have made a mistake!\"\n"
            ++ "     Generally speaking, you do not want it to have to be this way."
          )

    RedundantPatternMatch ->
        Report.simple
          "redundant pattern"
          "The following pattern is redundant."
          "Any value with this shape will be handled by a previous pattern."


-- TO JSON

toJson :: P.Dealiaser -> FilePath -> A.Located Warning -> Json.Value
toJson dealiaser filePath (A.A region warning) =
  let
    (maybeRegion, additionalFields) =
        Report.toJson [] (toReport dealiaser warning)
  in
      Json.object $
        [ "file" .= filePath
        , "region" .= maybe region id maybeRegion
        , "type" .= ("warning" :: String)
        ]
        ++ additionalFields


-- PATTERN WARNINGS

viewPatternList :: [Pattern.Pattern] -> String
viewPatternList unhandledPatterns =
  let
    (showPatterns, rest) =
      splitAt 4 unhandledPatterns
  in
    concatMap ((++) "\n    ") $
      map Pattern.toString showPatterns
      ++ if null rest then [] else ["..."]
