{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit.Init
  ( Exit(..)
  , toReport
  )
  where


import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help



-- EXITS


data Exit
  = ClashingFlags



-- TO REPORT


toReport :: Exit -> Help.Report
toReport exit =
  case exit of
    ClashingFlags ->
      Help.report "CLASHING FLAGS" Nothing
        "Trim down to zero or one flag. If you are not sure which one you want, learn more about them with:"
        [ D.indent 4 $ D.green $ "elm init --help"
        ]
