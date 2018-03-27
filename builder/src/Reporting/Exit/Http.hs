{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit.Http
  ( Exit(..)
  , toReport
  , BadJson(..)
  , badJsonToDocs
  )
  where


import qualified Data.Text as Text
import Text.PrettyPrint.ANSI.Leijen ((<>))
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Reporting.Exit.Help as Help



-- EXITS


data Exit
  = Unknown String
  | BadJson P.Doc
  | BadZipData
  | BadZipSha String String



-- TO REPORT


toReport :: String -> Exit -> Help.Report
toReport url exit =
  let
    urlDoc =
      P.indent 4 $ P.dullyellow $ "<" <> P.text url <> ">"
  in
  case exit of
    Unknown message ->
      Help.report "HTTP PROBLEM" Nothing "The following HTTP request failed:"
        [ urlDoc
        , Help.stack
            [ "Here is the error message I was able to extract:"
            , P.indent 4 $ Help.reflow message
            ]
        ]

    BadJson jsonErrorDoc ->
      Help.compilerReport jsonErrorDoc

    BadZipData ->
      Help.report "CORRUPT ZIP" Nothing "I could not unzip the file downloaded from:"
        [ urlDoc
        , Help.reflow $
            "If it is a transient issue, it should be fixed if you try this\
            \ again. If it seems like an Elm problem, please report it though!"
        ]

    BadZipSha expectedHash actualHash ->
      Help.report "CORRUPT ZIP" Nothing "I got an unexpected zip file from:"
        [ urlDoc
        , Help.reflow $
            "I was expecting the hash of content to be " ++ expectedHash
            ++ ", but it is " ++ actualHash
        , Help.reflow $
            "Most likely the package author may have moved the version\
            \ tag, so report it to them and see if that is the issue."
        ]



-- BAD JSON


data BadJson
  = BadNewPkg Text.Text
  | BadAllPkg Text.Text String
  | BadAllVsn Text.Text


badJsonToDocs :: BadJson -> [P.Doc]
badJsonToDocs badJson =
  case badJson of
    BadNewPkg txt ->
      ["I","ran","into",P.red (P.text (show txt)) <> ","
      ,"but","I","need","entries","like"
      ,P.green "\"elm-lang/core@6.0.0\"" <> "."
      ]

    BadAllPkg txt suggestion ->
      ["The",P.red (P.text (show txt)),"value","is","not","a","valid","package","name."
      ]
      ++ map P.text (words suggestion)

    BadAllVsn txt ->
      ["You","provided",P.red (P.text (show txt))
      ,"which","is","not","a","valid","version.","I","need","something","like"
      ,P.green "\"1.0.0\"","or",P.green "\"2.0.4\"" <> "."
      ]

