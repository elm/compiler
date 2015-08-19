module Generate.JavaScript.Crash where

import Language.ECMAScript3.Syntax (Expression)

import Generate.JavaScript.Helpers (call, obj, ref, string)
import qualified Reporting.Crash as Crash
import qualified Reporting.Region as R


localModuleName :: String
localModuleName =
    "$moduleName"


crash :: Crash.Details -> Expression ()
crash details =
  case details of
    Crash.IncompleteMultiIf region ->
        obj ["_U","badIf"]
          `call`
            [ ref localModuleName
            , string (R.toString region)
            ]

    Crash.IncompletePatternMatch _region ->
        obj ["_U","badCase"]
          `call`
            [ ref localModuleName
            , string "bugs in reporting the exact location right now"  -- TODO
            ]


