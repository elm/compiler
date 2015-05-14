{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json

import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Canonicalize
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Error.Type as Type
import qualified Reporting.Report as Report


-- ALL POSSIBLE ERRORS

data Error
    = Syntax Syntax.Error
    | Canonicalize Canonicalize.Error
    | Type Type.Error


-- TO STRING

toString :: String -> String -> A.Located Error -> String
toString location source (A.A region err) =
  let
    report =
        case err of
          Syntax syntaxError ->
              Syntax.toReport syntaxError

          Canonicalize canonicalizeError ->
              Canonicalize.toReport canonicalizeError

          Type typeError ->
              Type.toReport typeError
  in
      Report.toString location region report source


-- TO JSON

toJson :: FilePath -> A.Located Error -> Json.Value
toJson filePath (A.A region err) =
  let
    (maybeRegion, additionalFields) =
        case err of
          Syntax syntaxError ->
              Report.toJson [] (Syntax.toReport syntaxError)

          Canonicalize canonicalizeError ->
              let
                suggestions =
                  maybe []
                      (\s -> ["suggestions" .= s])
                      (Canonicalize.extractSuggestions canonicalizeError)
              in
                Report.toJson suggestions (Canonicalize.toReport canonicalizeError)

          Type typeError ->
              Report.toJson [] (Type.toReport typeError)
  in
      Json.object $
        [ "file" .= filePath
        , "region" .= maybe region id maybeRegion
        , "type" .= ("error" :: String)
        ]
        ++ additionalFields
