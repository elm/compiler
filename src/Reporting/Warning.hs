module Reporting.Warning where

import qualified Text.PrettyPrint as P
import Text.PrettyPrint ((<+>))

import qualified AST.Module as Module
import qualified AST.Type as Type
import qualified Reporting.Annotation as A
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Report as Report


-- ALL POSSIBLE WARNINGS

data Warning
    = UnusedImport Module.Name
    | MissingTypeAnnotation String Type.Canonical


-- TO STRING

toString :: String -> String -> A.Located Warning -> String
toString location source (A.A region warning) =
    Report.toString location region (toReport warning) source


toReport :: Warning -> Report.Report
toReport warning =
  case warning of
    UnusedImport moduleName ->
        Report.simple
          "warning"
          ("Module '" ++ Module.nameToString moduleName ++ "' is unused.")
          ""

    MissingTypeAnnotation name inferredType ->
        Report.simple
          "warning"
          ("Top-level value '" ++ name ++ "' does not have a type annotation.")
          ( "The type annotation you want looks something like this:\n\n"
            ++ P.render (P.nest 4 typeDoc)
          )
      where
        typeDoc =
          P.hang
            (P.text name <+> P.colon)
            4
            (P.pretty False inferredType)
