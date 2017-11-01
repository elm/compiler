{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Warning
  ( Warning(..)
  , Unused(..)
  , toReport
  , toJson
  )
  where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import Data.Text (Text)

import qualified AST.Type as Type
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Report as Report
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Helpers as Help
import Reporting.Helpers ((<>), text)



-- ALL POSSIBLE WARNINGS


data Warning
  = UnusedImport N.Name
  | UnusedVariable Unused N.Name
  | MissingTypeAnnotation Text Type.Canonical


data Unused = Pattern | Binding



-- TO REPORT


toReport :: RenderType.Localizer -> Warning -> Report.Report
toReport localizer warning =
  case warning of
    UnusedImport moduleName ->
        Report.report
          "unused import"
          Nothing
          ("Nothing from the `" <> moduleName <> "` module is used in this file.")
          (text "I recommend removing unused imports.")

    UnusedVariable Pattern name ->
        Report.report
          "unused variable"
          Nothing
          ("The `" <> name <> "` variable is unused.")
          ( Help.reflowParagraph $
              "Maybe this indicates there is a mistake around here? Switching `"
              <> name <> "` to _ will indicate that it is intentionally unused."
          )

    UnusedVariable Binding name ->
        Report.report
          "unused binding"
          Nothing
          ("The `" <> name <> "` binding is unused.")
          ( Help.reflowParagraph $
              "Maybe this indicates there is a mistake around here? Or maybe it\
              \ just is not used anymore and should be removed?"
          )

    MissingTypeAnnotation name inferredType ->
        Report.report
          "missing type annotation"
          Nothing
          ("Top-level value " <> Help.functionName name <> " does not have a type annotation.")
          ( Help.stack
              [ text "I inferred the type annotation so you can copy it into your code:"
              , RenderType.annotation localizer name inferredType
              ]
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
        , "type" .= ("warning" :: Text)
        ]
        ++ additionalFields
