{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Warning where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json

import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified Reporting.Annotation as A
import qualified Reporting.Report as Report
import qualified Reporting.Render.Type as RenderType


-- ALL POSSIBLE WARNINGS

data Warning
    = UnusedImport ModuleName.Raw
    | MissingTypeAnnotation String Type.Canonical


-- TO STRING

toString :: RenderType.Localizer -> String -> String -> A.Located Warning -> String
toString localizer location source (A.A region warning) =
    Report.toString location region (toReport localizer warning) source


print :: RenderType.Localizer -> String -> String -> A.Located Warning -> IO ()
print localizer location source (A.A region warning) =
    Report.printWarning location region (toReport localizer warning) source


toReport :: RenderType.Localizer -> Warning -> Report.Report
toReport localizer warning =
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
            ++ RenderType.annotation name inferredType
          )



-- TO JSON

toJson :: RenderType.Localizer -> FilePath -> A.Located Warning -> Json.Value
toJson localizer filePath (A.A region warning) =
  let
    (maybeRegion, additionalFields) =
        Report.toJson [] (toReport localizer warning)
  in
      Json.object $
        [ "file" .= filePath
        , "region" .= maybe region id maybeRegion
        , "type" .= ("warning" :: String)
        ]
        ++ additionalFields
