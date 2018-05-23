{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit.Http
  ( Exit(..)
  , toReport
  , BadJson(..)
  , badJsonToDocs
  )
  where


import qualified Data.Text as Text

import Reporting.Doc ((<>))
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help



-- EXITS


data Exit
  = Unknown String
  | BadJson String D.Doc
  | BadZipData
  | BadZipSha String String



-- TO REPORT


toReport :: String -> Exit -> Help.Report
toReport url exit =
  let
    urlDoc =
      D.indent 4 $ D.dullyellow $ "<" <> D.fromString url <> ">"
  in
  case exit of
    Unknown message ->
      Help.report "HTTP PROBLEM" Nothing "The following HTTP request failed:"
        [ urlDoc
        , D.stack
            [ "Here is the error message I was able to extract:"
            , D.indent 4 $ D.reflow message
            ]
        ]

    BadJson path doc ->
      Help.jsonReport "UNEXPECTED JSON" (Just path) doc

    BadZipData ->
      Help.report "CORRUPT ZIP" Nothing "I could not unzip the file downloaded from:"
        [ urlDoc
        , D.reflow $
            "If it is a transient issue, it should be fixed if you try this\
            \ again. If it seems like an Elm problem, please report it though!"
        ]

    BadZipSha expectedHash actualHash ->
      Help.report "CORRUPT ZIP" Nothing "I got an unexpected zip file from:"
        [ urlDoc
        , D.reflow "I check the hash the zip, and it seems off:"
        , D.vcat $ map D.fromString $
            [ "  Expected: " ++ expectedHash
            , "    Actual: " ++ actualHash
            ]
        , D.reflow $
            "This usually means that the package author moved the version\
            \ tag, so report it to them and see if that is the issue. Folks\
            \ on Elm slack can probably help as well."
        ]



-- BAD JSON


data BadJson
  = BadNewPkg Text.Text
  | BadAllPkg Text.Text String
  | BadAllVsn Text.Text


badJsonToDocs :: BadJson -> [D.Doc]
badJsonToDocs badJson =
  case badJson of
    BadNewPkg txt ->
      ["I","ran","into",D.red (D.fromString (show txt)) <> ","
      ,"but","I","need","entries","like"
      ,D.green "\"elm/core@6.0.0\"" <> "."
      ]

    BadAllPkg txt suggestion ->
      ["The",D.red (D.fromString (show txt)),"value","is","not","a","valid","package","name."
      ]
      ++ map D.fromString (words suggestion)

    BadAllVsn txt ->
      ["You","provided",D.red (D.fromString (show txt))
      ,"which","is","not","a","valid","version.","I","need","something","like"
      ,D.green "\"1.0.0\"","or",D.green "\"2.0.4\"" <> "."
      ]

