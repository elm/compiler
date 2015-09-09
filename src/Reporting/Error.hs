{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import Prelude hiding (print)

import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Canonicalize
import qualified Reporting.Error.Docs as Docs
import qualified Reporting.Error.Pattern as Pattern
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Error.Type as Type
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Report as Report


-- ALL POSSIBLE ERRORS

data Error
    = Syntax Syntax.Error
    | Canonicalize Canonicalize.Error
    | Type Type.Error
    | Pattern Pattern.Error
    | Docs Docs.Error


-- TO REPORT

toReport :: P.Dealiaser -> Error -> Report.Report
toReport dealiaser err =
  case err of
    Syntax syntaxError ->
        Syntax.toReport dealiaser syntaxError

    Canonicalize canonicalizeError ->
        Canonicalize.toReport dealiaser canonicalizeError

    Type typeError ->
        Type.toReport dealiaser typeError

    Pattern patternError ->
        Pattern.toReport dealiaser patternError

    Docs docsError ->
        Docs.toReport docsError


-- TO STRING

toString :: P.Dealiaser -> String -> String -> A.Located Error -> String
toString dealiaser location source (A.A region err) =
  Report.toString location region (toReport dealiaser err) source


print :: P.Dealiaser -> String -> String -> A.Located Error -> IO ()
print dealiaser location source (A.A region err) =
  Report.printError location region (toReport dealiaser err) source


-- TO JSON

toJson :: P.Dealiaser -> FilePath -> A.Located Error -> Json.Value
toJson dealiaser filePath (A.A region err) =
  let
    (maybeRegion, additionalFields) =
        case err of
          Syntax syntaxError ->
              Report.toJson [] (Syntax.toReport dealiaser syntaxError)

          Canonicalize canonicalizeError ->
              let
                suggestions =
                  maybe []
                      (\s -> ["suggestions" .= s])
                      (Canonicalize.extractSuggestions canonicalizeError)
              in
                Report.toJson suggestions (Canonicalize.toReport dealiaser canonicalizeError)

          Type typeError ->
              Report.toJson [] (Type.toReport dealiaser typeError)

          Pattern patternError ->
              Report.toJson [] (Pattern.toReport dealiaser patternError)

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
