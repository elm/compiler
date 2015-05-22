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


-- TO STRING

toString :: String -> String -> A.Located Warning -> String
toString location source (A.A region warning) =
    Report.toString location region (toReport warning) source


print :: String -> String -> A.Located Warning -> IO ()
print location source (A.A region warning) =
    Report.printWarning location region (toReport warning) source


toReport :: Warning -> Report.Report
toReport warning =
  case warning of
    UnusedImport moduleName ->
        Report.simple
          "unused import"
          ("Module '" ++ Module.nameToString moduleName ++ "' is unused.")
          ""

    MissingTypeAnnotation name inferredType ->
        Report.simple
          "missing type annotation"
          ("Top-level value '" ++ name ++ "' does not have a type annotation.")
          ( "The type annotation you want looks something like this:\n\n"
            ++ P.render (P.nest 4 typeDoc)
          )
      where
        typeDoc =
          P.hang
            (P.text name <+> P.colon)
            4
            (P.pretty False inferredType)


-- TO JSON

toJson :: FilePath -> A.Located Warning -> Json.Value
toJson filePath (A.A region warning) =
  let
    (maybeRegion, additionalFields) =
        Report.toJson [] (toReport warning)
  in
      Json.object $
        [ "file" .= filePath
        , "region" .= maybe region id maybeRegion
        , "type" .= ("warning" :: String)
        ]
        ++ additionalFields
