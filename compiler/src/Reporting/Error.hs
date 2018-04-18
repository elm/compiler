{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error
  ( Error(..)
  , toReports
  )
  where


import qualified Reporting.Error.Canonicalize as Canonicalize
import qualified Reporting.Error.Docs as Docs
import qualified Reporting.Error.Main as Main
import qualified Reporting.Error.Pattern as Pattern
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Error.Type as Type
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Report as Report



-- ALL POSSIBLE ERRORS


data Error
  = Syntax Syntax.Error
  | Canonicalize Canonicalize.Error
  | Type L.Localizer [Type.Error]
  | Main L.Localizer Main.Error
  | Pattern [Pattern.Error]
  | Docs Docs.Error



-- TO REPORT


toReports :: Code.Source -> Error -> [Report.Report]
toReports source err =
  case err of
    Syntax syntaxError ->
        [Syntax.toReport source syntaxError]

    Canonicalize canonicalizeError ->
        [Canonicalize.toReport source canonicalizeError]

    Type localizer typeErrors ->
        map (Type.toReport source localizer) typeErrors

    Main localizer mainError ->
        [Main.toReport localizer source mainError]

    Pattern patternErrors ->
        map (Pattern.toReport source) patternErrors

    Docs docsError ->
        [Docs.toReport source docsError]
