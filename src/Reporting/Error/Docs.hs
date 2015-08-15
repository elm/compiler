{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Docs where

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
        Report.simple "DOCUMENTATION ERROR"
          ( "You must have a documentation comment between the module declaration and the\n"
            ++ "imports."
          )
          "Learn how at <http://package.elm-lang.org/help/documentation-format>"

    OnlyInDocs name suggestions ->
        Report.simple "DOCUMENTATION ERROR"
          ("Your module documentation includes `" ++ name ++ "` which is not exported.")
          ("Is it misspelled? Should it be exported? " ++ Help.maybeYouWant suggestions)

    OnlyInExports names ->
        Report.simple
          "DOCUMENTATION ERROR"
          ( "The following exports do not appear in your module documentation:\n"
            ++ concatMap ("\n    " ++) names
          )
          ( "All exports must be listed in the module documentation after a @docs keyword.\n"
            ++ "Learn how at <http://package.elm-lang.org/help/documentation-format>"
          )

    NoComment name ->
        Report.simple "DOCUMENTATION ERROR"
          ("The value `" ++ name ++ "` does not have a documentation comment.")
          ( "Documentation comments start with {-| and end with -}. They should provide a\n"
            ++ "clear description of how they work, and ideally a small code example. This is\n"
            ++ "extremely valuable for users checking out your package!\n\n"
            ++ "If you think the docs are clearer without any words, you can use an empty\n"
            ++ "comment {-|-} which should be used sparingly. Maybe you have a section of 20\n"
            ++ "values all with the exact same type. The docs may read better if they are all\n"
            ++ "described in one place.\n\n"
            ++ "Learn more at <http://package.elm-lang.org/help/documentation-format>"
          )

    NoType name ->
        Report.simple "MISSING ANNOTATION"
          ("The value `" ++ name ++ "` does not have a type annotation.")
          ( "Adding type annotations is best practice and it gives you a chance to name\n"
            ++ "types and type variables so they are as easy as possible to understand!"
          )
