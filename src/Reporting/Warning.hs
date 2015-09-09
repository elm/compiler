{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Warning where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Text.PrettyPrint as P
import Text.PrettyPrint ((<+>))

import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified Reporting.Annotation as A
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Report as Report


-- ALL POSSIBLE WARNINGS

data Warning
    = UnusedImport ModuleName.Raw
    | MissingTypeAnnotation String Type.Canonical


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
          ("Module `" ++ ModuleName.toString moduleName ++ "` is unused.")
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
