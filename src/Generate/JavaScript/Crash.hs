module Generate.JavaScript.Crash where

import Language.ECMAScript3.Syntax (Expression)

import Generate.JavaScript.Helpers (call, obj, ref, string)
import qualified Reporting.Crash as Crash
import qualified Reporting.Region as R


localModuleName :: String
localModuleName =
    "$moduleName"


crash :: R.Region -> Crash.Details -> Expression ()
crash region details =
  case details of
    Crash.IncompleteMultiIf _ ->
        obj ["_U","badIf"]
          `call`
            [ ref localModuleName
            , string (R.toString region)
            ]

    Crash.IncompletePatternMatch ->
        obj ["_U","badCase"]
          `call`
            [ ref localModuleName
            , string (R.toString region)
            ]


