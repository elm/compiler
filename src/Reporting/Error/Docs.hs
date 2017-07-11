{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Docs where

import Data.Text (Text)

import qualified Reporting.Report as Report
import qualified Reporting.Helpers as Help
import Reporting.Helpers ((<>), text)



data Error
    = NoDocs
    | OnlyInDocs Text [Text]
    | OnlyInExports [Text]
    | Duplicates Text
    | NoComment Text
    | NoType Text



-- TO REPORT


toReport :: Error -> Report.Report
toReport err =
  case err of
    NoDocs ->
        Report.report
          "DOCUMENTATION ERROR"
          Nothing
          ( "You must have a documentation comment between the module\
            \ declaration and the imports."
          )
          ( text "Learn more at <http://package.elm-lang.org/help/documentation-format>"
          )

    OnlyInDocs name suggestions ->
        Report.report
          "DOCUMENTATION ERROR"
          Nothing
          ("Your module documentation includes `" <> name <> "` which is not exported."
          )
          ( Help.maybeYouWant (Just "Is it misspelled? Should it be exported?") suggestions
          )

    OnlyInExports names ->
        Report.report
          "DOCUMENTATION ERROR"
          Nothing
          ( "The following exports do not appear in your module documentation: "
            <> Help.commaSep names
          )
          ( text $
              "All exports must be listed in the module documentation after a @docs keyword.\n"
              <> "Learn more at <http://package.elm-lang.org/help/documentation-format>"
          )

    Duplicates name ->
        Report.report
          "DOCUMENTATION ERROR"
          Nothing
          ( "There can only be one `" <> name <> "` in your module documentation."
          )
          ( text "Remove one of them!"
          )

    NoComment name ->
        Report.report
          "DOCUMENTATION ERROR"
          Nothing
          ("The `" <> name <> "` definition does not have a documentation comment.")
          ( text "Learn more at <http://package.elm-lang.org/help/documentation-format>"
          )

    NoType name ->
        Report.report
          "MISSING ANNOTATION"
          Nothing
          ("The `" <> name <> "` definition does not have a type annotation.")
          ( text $
              "Adding type annotations is best practice and it gives you a chance to name\n"
              <> "types and type variables so they are as easy as possible to understand!"
          )
