{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Warning
  ( Warning(..)
  , Context(..)
  , toReport
  , toJson
  )
  where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import Data.Text (Text)

import qualified AST.Canonical as Can
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import qualified Reporting.Report as Report
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Helpers as Help
import Reporting.Helpers ((<>), text)



-- ALL POSSIBLE WARNINGS


data Warning
  = UnusedImport N.Name
  | UnusedVariable R.Region Context N.Name
  | MissingTypeAnnotation Text Can.Type


data Context = Def | Pattern



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

    UnusedVariable _ context name ->
      Report.report
        (defOrPat context "unused definition" "unused variable")
        Nothing
        ("You are not using `" <> name <> "` anywhere.")
        ( Help.stack
            [ Help.reflowParagraph $
                "Is there a typo? Maybe you intended to use `" <> name
                <> "` but typed a similar name instead?"
            , Help.reflowParagraph $
                defOrPat context
                  ( "If you are sure there is no typo, remove the definition.\
                    \ This way future readers will not have to wonder!"
                  )
                  ( "If you are sure there is no typo, replace `" <> name
                    <> "` with _ so future readers will not have to wonder!"
                  )
            ]
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


defOrPat :: Context -> a -> a -> a
defOrPat context def pat =
  case context of
    Def -> def
    Pattern -> pat



-- TO JSON


toJson :: RenderType.Localizer -> FilePath -> A.Located Warning -> Json.Value
toJson localizer filePath (A.At region warning) =
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
