{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Nitpick
  ( Error(..)
  --
  , toReport
  )
  where


import qualified Data.List as List

import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Render.Code as Code
import qualified Reporting.Report as Report
import qualified Reporting.Suggest as Suggest



-- ERROR


data Error
  = ExportDuplicate_Op  Op.Name A.Region A.Region
  | ExportDuplicate_Var  N.Name A.Region A.Region
  | ExportDuplicate_Type T.Name A.Region A.Region
  | ExportNotFound_Op   A.Region Op.Name [Op.Name]
  | ExportNotFound_Var  A.Region  N.Name [N.Name]
  | ExportNotFound_Type A.Region  T.Name [T.Name]
  | ExportOpenAlias A.Region T.Name



-- TO REPORT


toReport :: Code.Source -> Error -> Report.Report
toReport source err =
  case err of
    ExportDuplicate_Op   n r1 r2 -> exportDuplicate source n r1 r2 Op.toChars
    ExportDuplicate_Var  n r1 r2 -> exportDuplicate source n r1 r2 N.toChars
    ExportDuplicate_Type n r1 r2 -> exportDuplicate source n r1 r2 T.nameToChars
    ExportNotFound_Op    r n  ns -> exportNotFound  source r n  ns Op.toChars    badOp
    ExportNotFound_Var   r n  ns -> exportNotFound  source r n  ns N.toChars     badVar
    ExportNotFound_Type  r n  ns -> exportNotFound  source r n  ns T.nameToChars badType

    ExportOpenAlias region name ->
      Report.Report "OVERLY EXPOSED TYPE ALIAS" region [] $
        Code.toSnippet source region Nothing
          (
            D.reflow $
              "The (..) syntax is for exposing variants of a custom type. It cannot be used with a type alias like `"
              ++ T.nameToChars name ++ "` though."
          ,
            D.reflow $
              "Remove the (..) and you should be fine!"
          )



-- EXPORT DUPLICATE


exportDuplicate :: Code.Source -> name -> A.Region -> A.Region -> (name -> String) -> Report.Report
exportDuplicate source name r1 r2 toChars =
  let
    messageThatEndsWithPunctuation =
      "You are trying to expose `" <> toChars name <> "` multiple times!"
  in
  Report.Report "REDUNDANT EXPORT" r2 [] $
    Code.toPair source r1 r2
      (
        D.reflow messageThatEndsWithPunctuation
      ,
        "Remove one of them and you should be all set!"
      )
      (
        D.reflow (messageThatEndsWithPunctuation <> " Once here:")
      ,
        "And again right here:"
      ,
        "Remove one of them and you should be all set!"
      )



-- EXPORT NOT FOUND


type BadThing name =
  name -> ( D.Doc, D.Doc, D.Doc )

badOp   :: BadThing Op.Name
badVar  :: BadThing N.Name
badType :: BadThing T.Name

badOp   n = ( "an", "operator", "(" <> D.fromOp   n <> ")" )
badVar  n = ( "a" , "value"   , "`" <> D.fromName n <> "`" )
badType n = ( "a" , "type"    , "`" <> D.fromType n <> "`" )


exportNotFound :: Code.Source -> A.Region -> name -> [name] -> (name -> String) -> BadThing name -> Report.Report
exportNotFound source region rawName possibleNames toChars badThing =
  let
    suggestions =
      List.map toChars $ List.take 4 $
        Suggest.sort (toChars rawName) toChars possibleNames

    (a, thing, name) = badThing rawName
  in
  Report.Report "UNKNOWN EXPORT" region suggestions $
    Code.toSnippet source region Nothing
      (
        D.fillSep
          ["You","are","trying","to","expose",a,thing,"named"
          ,name,"but","I","cannot","find","its","definition."
          ]
      ,
        case List.map D.fromChars suggestions of
          [] ->
            D.reflow $
              "I do not see any super similar names in this file. Is the definition missing?"

          [alt] ->
            D.fillSep ["Maybe","you","want",D.dullyellow alt,"instead?"]

          alts ->
            D.stack
              [ "These names seem close though:"
              , D.indent 4 $ D.vcat $ List.map D.dullyellow alts
              ]
      )

