{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit.Assets
  ( Exit(..)
  , ElmJsonProblem(..)
  , toReport
  , BadElmJsonContent(..)
  , badContentToDocs
  )
  where


import qualified Data.Text as Text

import qualified Elm.Package as Pkg
import Reporting.Doc ((<>))
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help



-- EXITS


data Exit
  = BadElmJson ElmJsonProblem
  | CorruptElmJson Pkg.Name Pkg.Version
  | CorruptDocumentation Pkg.Name Pkg.Version
  | CorruptBinary FilePath FilePath



-- ELM JSON PROBLEM


data ElmJsonProblem
  = BadJson D.Doc
  | BadSrcDir FilePath
  | NoPkgCore
  | NoAppCore
  | NoAppJson



-- TO REPORT


toReport :: Exit -> Help.Report
toReport exit =
  case exit of
    CorruptElmJson pkg vsn ->
      corruptJsonToReport "elm.json" pkg vsn

    CorruptDocumentation pkg vsn ->
      corruptJsonToReport "docs.json" pkg vsn

    CorruptBinary elmHome path ->
      Help.report "CORRUPT BINARY" (Just path)
        ("The binary data at " ++ path ++ " is corrupt.")
        [ D.reflow "Elm caches build artifacts in the following directories:"
        , D.dullyellow $ D.indent 4 $ D.vcat $
            [ D.fromString elmHome
            , "elm-stuff/"
            ]
        , D.reflow
            "Maybe you recently installed a command line tool or editor plugin that messes\
            \ with them? They definitely should not be doing that, but you never know! So\
            \ maybe try deleting them? Everything will be rebuilt from scratch. This may\
            \ help reveal the corrupting influence."
        ]

    BadElmJson problem ->
      elmJsonProblemToReport problem



-- CORRUPT JSON


corruptJsonToReport :: FilePath -> Pkg.Name -> Pkg.Version -> Help.Report
corruptJsonToReport path pkg vsn =
  Help.report "CORRUPT JSON" Nothing
    ( "The " ++ path ++ " for " ++ Pkg.toString pkg
      ++ " " ++ Pkg.versionToString vsn ++ " got corrupted somehow."
    )
    [ D.reflow $
        "I removed it from my file cache, so if it was some transient\
        \ error it should be fixed if you try the same thing again.\
        \ Please report this if it seems like an Elm problem though!"
    ]



-- BAD ELM JSON


elmJsonProblemToReport :: ElmJsonProblem -> Help.Report
elmJsonProblemToReport problem =
  case problem of
    BadJson doc ->
      Help.jsonReport "BAD JSON" (Just "elm.json") doc

    BadSrcDir dir ->
      Help.report "BAD JSON" (Just "elm.json")
        "The \"source-directories\" in your elm.json lists the following directory:"
        [ D.indent 4 (D.dullyellow (D.fromString dir))
        , D.reflow "I cannot find that directory though! Is it missing? Is there a typo?"
        ]

    NoPkgCore ->
      Help.report "MISSING DEPENDENCY" (Just "elm.json")
        "A package must have \"elm/core\" as a dependency. Try running:"
        [ D.indent 4 $ D.green $ "elm install elm/core"
        , D.reflow "I need it for the default imports that make `List` and `Maybe` available."
        ]

    NoAppCore ->
      Help.report "MISSING DEPENDENCY" (Just "elm.json")
        "An application must have \"elm/core\" as a dependency. Try running:"
        [ D.indent 4 $ D.green $ "elm install elm/core"
        , D.reflow "It has some supporting code that is needed by every Elm application!"
        ]

    NoAppJson ->
      Help.report "MISSING DEPENDENCY" (Just "elm.json")
        "An application must have \"elm/json\" as a dependency. Try running:"
        [ D.indent 4 $ D.green $ "elm install elm/json"
        , D.reflow "It helps me handle flags and ports."
        ]



-- PROBLEM TO DOCS


data BadElmJsonContent
  = BadType Text.Text
  | BadPkgName String
  | BadVersion Text.Text
  | BadConstraint String
  | BadModuleName Text.Text
  | BadModuleHeaderTooLong Text.Text
  | BadDependencyName Text.Text
  | BadLicense Text.Text [Text.Text]
  | BadDirectoryNotString
  | BadSummaryTooLong


badContentToDocs :: BadElmJsonContent -> [D.Doc]
badContentToDocs badContent =
  case badContent of
    BadType _ ->
      ["The","only","valid","types","of","elm.json","are"
      ,D.green "\"application\"","and",D.green "\"package\"" <> "."
      ]

    BadPkgName problem ->
      map D.fromString (words problem)

    BadVersion txt ->
      ["You","provided",D.red (D.fromString (show txt))
      ,"which","is","not","a","valid","version.","I","need","something","like"
      ,D.green "\"1.0.0\"","or",D.green "\"2.0.4\"" <> "."
      ]

    BadConstraint problem ->
      map D.fromString (words ("It is not a valid constraint. " ++ problem))

    BadModuleName name ->
      ["You","provided",D.fromString (show name),"which","is","not","a","valid","module","name."
      ,"I","need","something","like",D.green "\"Html.Events\"","or",D.green "\"Browser.Navigation\"" <> "."
      ]

    BadModuleHeaderTooLong header ->
      ["The",D.fromString (show header),"header","is","too","long."
      ,"I","need","it","to","be",D.green "under",D.green "20",D.green "characters"
      ,"to","ensure","that","formatting","is","nice","on","the","package","website."
      ]

    BadDependencyName name ->
      ["The",D.fromString (show name),"entry","is","not","a","valid","package","name."
      ,"I","recommend","deleting","it,","finding","the","package","you","want","on"
      ,"the","package","website,","and","installing","it","with","the"
      ,D.green "`elm install`","command","instead."
      ]

    BadLicense _given suggestions ->
      case suggestions of
        [] ->
          ["I","need","an","OSI","approved","SPDX","license,","like"
          ,D.green "\"BSD-3-Clause\"","or",D.green "\"MIT\".","See"
          ,"<https://spdx.org/licenses/>","for","a","full","list","of","options."
          ]

        code:codes ->
          ["I","need","an","OSI","approved","SPDX","license."]
          ++ ["Maybe you wanted"] ++ oneOf code codes ++ ["instead?"]
          ++ ["See","<https://spdx.org/licenses/>","for","a","full","list","of","options."]

    BadDirectoryNotString ->
      ["I","need","a",D.green "STRING","that","points","to","a","directory","of","Elm","code."
      ]

    BadSummaryTooLong ->
      ["Your","summary","is","too","long.","I","need","it","to"
      ,"be",D.green "under",D.green "80",D.green "characters."
      ]


oneOf :: Text.Text -> [Text.Text] -> [D.Doc]
oneOf code codes =
  let
    toDoc spdx =
      D.green (D.fromString (show spdx))
  in
  case codes of
    [] ->
      [toDoc code]

    [code2] ->
      [toDoc code,"or",toDoc code2]

    _:_ ->
      let
        these = code : init codes
        final = last codes
      in
      map (\spdx -> toDoc spdx <> ",") these ++ ["or",toDoc final]
