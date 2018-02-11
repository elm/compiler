{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Warning
  ( Warning(..)
  , Context(..)
  , toReport
  )
  where


import Data.Monoid ((<>))

import qualified AST.Canonical as Can
import qualified AST.Utils.Type as Type
import qualified Elm.Name as N
import qualified Reporting.Region as R
import qualified Reporting.Report as Report
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type as RT
import qualified Reporting.Helpers as H



-- ALL POSSIBLE WARNINGS


data Warning
  = UnusedImport R.Region N.Name
  | UnusedVariable R.Region Context N.Name
  | MissingTypeAnnotation R.Region N.Name Can.Type


data Context = Def | Pattern



-- TO REPORT


toReport :: Code.Source -> Warning -> Report.Report
toReport source warning =
  case warning of
    UnusedImport region moduleName ->
      Report.Report "unused import" region [] $
        Report.toCodeSnippet source region Nothing
          (
            H.reflow $
              "Nothing from the `" <> N.toString moduleName <> "` module is used in this file."
          ,
            "I recommend removing unused imports."
          )

    UnusedVariable region context name ->
      let title = defOrPat context "unused definition" "unused variable" in
      Report.Report title region [] $
        Report.toCodeSnippet source region Nothing
          (
            H.reflow $
              "You are not using `" <> N.toString name <> "` anywhere."
          ,
            H.stack
              [ H.reflow $
                  "Is there a typo? Maybe you intended to use `" <> N.toString name
                  <> "` somewhere but typed another name instead?"
              , H.reflow $
                  defOrPat context
                    ( "If you are sure there is no typo, remove the definition.\
                      \ This way future readers will not have to wonder why it is there!"
                    )
                    ( "If you are sure there is no typo, replace `" <> N.toString name
                      <> "` with _ so future readers will not have to wonder why it is there!"
                    )
              ]
          )

    MissingTypeAnnotation region name inferredType ->
        Report.Report "missing type annotation" region [] $
          Report.toCodeSnippet source region Nothing
            (
              H.reflow $
                case Type.deepDealias inferredType of
                  Can.TLambda _ _ ->
                    "The `" <> N.toString name <> "` function has no type annotation."

                  _ ->
                    "The `" <> N.toString name <> "` definition has no type annotation."
            ,
              H.stack
                [ "I inferred the type annotation myself though! You can copy it into your code:"
                , H.green $ H.hang 4 $ H.sep $
                    [ H.nameToDoc name <> " :"
                    , RT.canToDoc RT.None inferredType
                    ]
                ]
            )


defOrPat :: Context -> a -> a -> a
defOrPat context def pat =
  case context of
    Def -> def
    Pattern -> pat

