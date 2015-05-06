{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error where

import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as Canonicalize
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Error.Type as Type
import qualified Reporting.Error.CheckMatch as CheckMatch
import qualified Reporting.Report as Report


-- ALL POSSIBLE ERRORS

data Error
    = Syntax Syntax.Error
    | Canonicalize Canonicalize.Error
    | Type Type.Error
    | CheckMatch CheckMatch.Error


-- TO STRING

toString :: String -> String -> A.Located Error -> String
toString location source (A.A region err) =
  let
    (tag, report) =
        case err of
          Syntax syntaxError ->
              ( "SYNTAX ERROR"
              , Syntax.toReport syntaxError
              )

          Canonicalize canonicalizeError ->
              ( "NAMING ERROR"
              , Canonicalize.toReport canonicalizeError
              )

          Type typeError ->
              ( "TYPE ERROR"
              , Type.toReport typeError
              )
  in
      Report.toString tag location region report source
