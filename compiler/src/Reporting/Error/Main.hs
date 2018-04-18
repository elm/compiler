{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Main
  ( Error(..)
  , toReport
  )
  where


import qualified AST.Canonical as Can
import qualified Elm.Name as N
import qualified Reporting.Doc as D
import qualified Reporting.Error.Canonicalize as E
import qualified Reporting.Region as R
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Report as Report



-- ERROR


data Error
  = BadType R.Region Can.Type
  | BadCycle R.Region [N.Name]
  | BadFlags R.Region Can.Type E.InvalidPayload



-- TO REPORT


toReport :: L.Localizer -> Code.Source -> Error -> Report.Report
toReport localizer source err =
  case err of
    BadType region tipe ->
      Report.Report "BAD MAIN TYPE" region [] $
        Report.toCodeSnippet source region Nothing
          (
            "I cannot handle this type of `main` value:"
          ,
            D.stack
              [ "The type of `main` value I am seeing is:"
              , D.indent 4 $ D.dullyellow $ RT.canToDoc localizer RT.None tipe
              , D.reflow $
                  "I only know how to handle Html, Svg, and Programs\
                  \ though. Modify `main` to be one of those types of values!"
              ]
          )

    BadCycle region cycleNames ->
      Report.Report "BAD MAIN" region [] $
        Report.toCodeSnippet source region Nothing
          (
            "A `main` definition cannot be defined in terms of itself."
          ,
            D.stack
              [ D.reflow $
                  "It should be a boring value with no recursion. But\
                  \ instead it is involved in this cycle of definitions:"
              , D.cycle 4 cycleNames
              ]
          )

    BadFlags region _badType invalidPayload ->
      let
        formatDetails (aBadKindOfThing, butThatIsNoGood) =
          Report.Report "BAD FLAGS" region [] $
            Report.toCodeSnippet source region Nothing
              (
                D.reflow $
                  "Your `main` program wants " ++ aBadKindOfThing ++ " from JavaScript."
              ,
                butThatIsNoGood
              )
      in
      formatDetails $
        case invalidPayload of
          E.ExtendedRecord ->
            (
              "an extended record"
            ,
              D.reflow $
                "But the exact shape of the record must be known at compile time. No type variables!"
            )

          E.Function ->
            (
              "a function"
            ,
              D.reflow $
                "But if I allowed functions from JS, it would be possible to sneak\
                \ side-effects and runtime exceptions into Elm!"
            )

          E.TypeVariable name ->
            (
              "an unspecified type"
            ,
              D.reflow $
                "But type variables like `" ++ N.toString name ++ "` cannot be given as flags.\
                \ I need to know exactly what type of data I am getting, so I can guarantee that\
                \ unexpected data cannot sneak in and crash the Elm program."
            )

          E.UnsupportedType name ->
            (
              "a `" ++ N.toString name ++ "` value"
            ,
              D.stack
                [ D.reflow $ "I cannot handle that. The types that CAN be in flags include:"
                , D.indent 4 $
                    D.reflow $
                      "Ints, Floats, Bools, Strings, Maybes, Lists, Arrays,\
                      \ tuples, records, and JSON values."
                , D.reflow $
                    "Since JSON values can flow through, you can use JSON encoders and decoders\
                    \ to allow other types through as well. More advanced users often just do\
                    \ everything with encoders and decoders for more control and better errors."
                ]
            )
