{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json

import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Canonicalize
import qualified Reporting.Error.Docs as Docs
import qualified Reporting.Error.Pattern as Pattern
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Error.Type as Type
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Report as Report



-- ALL POSSIBLE ERRORS


data Error
    = Syntax Syntax.Error
    | Canonicalize Canonicalize.Error
    | Type Type.Error
    | Pattern Pattern.Error
    | Docs Docs.Error



-- TO REPORT


toReport :: RenderType.Localizer -> Error -> Report.Report
toReport localizer err =
  case err of
    Syntax syntaxError ->
        Syntax.toReport localizer syntaxError

    Canonicalize canonicalizeError ->
        Canonicalize.toReport localizer canonicalizeError

    Type typeError ->
        Type.toReport localizer typeError

    Pattern patternError ->
        Pattern.toReport localizer patternError

    Docs docsError ->
        Docs.toReport docsError



-- TO JSON


toJson :: RenderType.Localizer -> FilePath -> A.Located Error -> Json.Value
toJson localizer filePath (A.A region err) =
  let
    (maybeRegion, additionalFields) =
        case err of
          Syntax syntaxError ->
              Report.toJson [] (Syntax.toReport localizer syntaxError)

          Canonicalize canonicalizeError ->
              let
                suggestions =
                  maybe []
                      (\s -> ["suggestions" .= s])
                      (Canonicalize.extractSuggestions canonicalizeError)
              in
                Report.toJson suggestions (Canonicalize.toReport localizer canonicalizeError)

          Type typeError ->
              Report.toJson [] (Type.toReport localizer typeError)

          Pattern patternError ->
              Report.toJson [] (Pattern.toReport localizer patternError)

          Docs docsError ->
              Report.toJson [] (Docs.toReport docsError)
  in
      Json.object $
        [ "file" .= filePath
        , "region" .= region
        , "subregion" .= maybeRegion
        , "type" .= ("error" :: String)
        ]
        ++ additionalFields
