{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Docs where

import Text.PrettyPrint.ANSI.Leijen (text)

import qualified Reporting.Error.Helpers as Help
import qualified Reporting.Report as Report



data Error
    = NoDocs
    | OnlyInDocs String [String]
    | OnlyInExports [String]
    | NoComment String
    | NoType String



-- TO REPORT


toReport :: Error -> Report.Report
toReport err =
  case err of
    NoDocs ->
        Report.report
          "DOCUMENTATION ERROR"
          Nothing
          ( "You must have a documentation comment between the module declaration and the\n"
            ++ "imports."
          )
          ( text "Learn more at <http://package.elm-lang.org/help/documentation-format>"
          )

    OnlyInDocs name suggestions ->
        Report.report
          "DOCUMENTATION ERROR"
          Nothing
          ("Your module documentation includes `" ++ name ++ "` which is not exported.")
          ( text $
              "Is it misspelled? Should it be exported? "
              ++ Help.maybeYouWant suggestions
          )

    OnlyInExports names ->
        Report.report
          "DOCUMENTATION ERROR"
          Nothing
          ( "The following exports do not appear in your module documentation:\n"
            ++ concatMap ("\n    " ++) names
          )
          ( text $
              "All exports must be listed in the module documentation after a @docs keyword.\n"
              ++ "Learn more at <http://package.elm-lang.org/help/documentation-format>"
          )

    NoComment name ->
        Report.report
          "DOCUMENTATION ERROR"
          Nothing
          ("The value `" ++ name ++ "` does not have a documentation comment.")
          ( text "Learn more at <http://package.elm-lang.org/help/documentation-format>"
          )

    NoType name ->
        Report.report
          "MISSING ANNOTATION"
          Nothing
          ("The value `" ++ name ++ "` does not have a type annotation.")
          ( text $
              "Adding type annotations is best practice and it gives you a chance to name\n"
              ++ "types and type variables so they are as easy as possible to understand!"
          )
