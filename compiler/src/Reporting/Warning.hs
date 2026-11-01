{-# LANGUAGE OverloadedStrings #-}
module Reporting.Warning
  ( Warning(..)
  , toReport
  )
  where


import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified Elm.Interface as I
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Report as Report
import qualified Reporting.Render.Code as Code
import qualified Reporting.Render.Type as RT



-- ALL POSSIBLE WARNINGS


data Warning
  = UnusedImport A.Region Module.Name
  | UnusedDef A.Region N.Name
  | UnusedVar A.Region N.Name
  | MissingTypeAnnotation (A.Located N.Name) I.Annotation



-- TO REPORT


toReport :: RT.Localizer -> Code.Source -> Warning -> Report.Report
toReport localizer source warning =
  case warning of
    UnusedImport region moduleName ->
      Report.Report "UNUSED IMPORT" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "Nothing from the `" <> Module.toChars moduleName <> "` module is used in this file."
          ,
            "I recommend removing unused imports."
          )

    UnusedDef region name ->
      Report.Report "UNUSED DEFINITION" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "You are not using `" <> N.toChars name <> "` anywhere."
          ,
            D.stack
              [ D.reflow $
                  "Is there a typo? Maybe you intended to use `" <> N.toChars name
                  <> "` somewhere but typed another name instead?"
              , D.reflow $
                  "If you are sure there is no typo, remove the definition.\
                  \ This way future readers will not have to wonder why it is there!"
              ]
          )

    UnusedVar region name ->
      Report.Report "UNUSED VARIABLE" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "You are not using `" <> N.toChars name <> "` anywhere."
          ,
            D.stack
              [ D.reflow $
                  "Is there a typo? Maybe you intended to use `" <> N.toChars name
                  <> "` somewhere but typed another name instead?"
              , D.reflow $
                  "If you are sure there is no typo, replace `" <> N.toChars name
                  <> "` with _ so future readers will not have to wonder why it is there!"
              ]
          )

    MissingTypeAnnotation (A.At region name) inferredType ->
        Report.Report "MISSING TYPE ANNOTATION" region [] $
          Code.toSnippet source region Nothing
            (
              D.reflow $
                if isLambda inferredType
                then "The `" <> N.toChars name <> "` function has no type annotation."
                else "The `" <> N.toChars name <> "` definition has no type annotation."
            ,
              D.stack
                [ "I inferred the type annotation myself though! You can copy it into your code:"
                , D.green $ D.hang 4 $ D.sep $
                    [ D.fromName name <> " :"
                    , RT.iToDoc localizer RT.None inferredType
                    ]
                ]
            )


isLambda :: I.Annotation -> Bool
isLambda (I.Forall _ tipe) =
    go tipe
  where
    go t =
      case t of
        I.TLambda _ _ _     -> True
        I.TVar _            -> False
        I.TType  _ _ _ _    -> False
        I.TAlias _ _ _ xs a -> go (I.dealias xs a)
        I.TUnit             -> False
        I.TPair   _ _       -> False
        I.TTriple _ _ _     -> False
        I.TEmpty            -> False
        I.TRecord _         -> False
        I.TExtRec _ _       -> False

