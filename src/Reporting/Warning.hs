{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Warning where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Text.PrettyPrint as P
import Text.PrettyPrint ((<+>))

import qualified AST.Module as Module
import qualified AST.Type as Type
import qualified Reporting.Annotation as A
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Report as Report


-- ALL POSSIBLE WARNINGS

data Warning
    = UnusedImport Module.Name
    | MissingTypeAnnotation String Type.Canonical
    | InexhaustivePatternMatch
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

    InexhaustivePatternMatch ->
        Report.simple
          "missing pattern"
          "The following case expression does not handle all possible inputs."
          ( "If none of the patterns match the given value, we have no choice but to crash!\n"
            ++ "\n"
            ++ "If you somehow know that this pattern can never happen, you want to:\n"
            ++ "\n"
            ++ "  1. Rethink the data structures you are using. Is there a way to model the\n"
            ++ "     values more precisely?\n"
            ++ "  2. End the pattern match with a wildcard match that leads to an expression\n"
            ++ "     like: Debug.crash \"This should never happen, but I guess it can!\""
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
