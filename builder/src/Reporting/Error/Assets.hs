{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Assets
  ( Error(..)
  , ElmJsonProblem(..)
  , toReport
  , BadElmJsonContent(..)
  , badContentToDocs
  )
  where


import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Elm.Package as Pkg
import qualified Reporting.Error.Help as Help



-- ERRORS


data Error
  = BadElmJson ElmJsonProblem
  | CorruptElmJson Pkg.Name Pkg.Version
  | CorruptDocumentation Pkg.Name Pkg.Version
  | CorruptBinary FilePath



-- ELM JSON PROBLEM


data ElmJsonProblem
  = BadJson P.Doc
  | BadDepDup String String Pkg.Name [Pkg.Name]
  | BadSrcDir FilePath



-- TO REPORT


toReport :: Error -> Help.Report
toReport err =
  case err of
    CorruptElmJson pkg vsn ->
      corruptJsonToReport "elm.json" pkg vsn

    CorruptDocumentation pkg vsn ->
      corruptJsonToReport "docs.json" pkg vsn

    CorruptBinary path ->
      Help.report "CORRUPT BINARY" (Just path)
        ("The binary data at " ++ path ++ " is corrupt.")
        [ Help.reflow $
            "Maybe a program is modifying your elm-stuff/ or ELM_HOME\
            \ directory in unexpected ways? Both of those are just caches, so\
            \ you can try deleting them and they will be rebuilt from scratch."
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
    [ Help.reflow $
        "I removed it from my file cache, so if it was some transient\
        \ error it should be fixed if you try the same thing again.\
        \ Please report this if it seems like an Elm problem though!"
    ]



-- BAD ELM JSON


elmJsonProblemToReport :: ElmJsonProblem -> Help.Report
elmJsonProblemToReport problem =
  case problem of
    BadJson jsonErrorDoc ->
      Help.compilerReport jsonErrorDoc

    BadSrcDir dir ->
      Help.report "BAD JSON" (Just "elm.json")
        "The \"source-directories\" in your elm.json lists the following directory:"
        [ P.indent 4 (P.dullyellow (P.text dir))
        , Help.reflow "I cannot find that directory though! Is it missing? Is there a typo?"
        ]

    BadDepDup field1 field2 dup dups ->
      let
        packagesAre =
          if null dups then "package is" else "packages are"

        advice =
          if null dups then
            "Delete one of the entries."
          else
            "Delete duplicates until each package lives in ONE category."
      in
        Help.report "BAD JSON" (Just "elm.json")
          (
            if null dups then
              "The following package appears twice in your elm.json file:"
            else
              "The following packages appear twice in your elm.json file:"
          )
          [ P.dullyellow $ P.indent 4 $ P.vcat $
              map (P.text . Pkg.toString) (dup:dups)
          , Help.reflow $
              "The " ++ packagesAre ++ " available in \"" ++ field1
              ++ "\", so it is redundant to list it again in \"" ++ field2
              ++ "\". It is already available! " ++ advice
          , Help.reflow "More help at <https://github.com/elm-lang/elm-package/blob/master/TODO>"
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


badContentToDocs :: BadElmJsonContent -> [P.Doc]
badContentToDocs badContent =
  case badContent of
    BadType _ ->
      ["The","only","valid","types","of","elm.json","are"
      ,P.green "\"application\"","and",P.green "\"package\"" <> "."
      ]

    BadPkgName problem ->
      map P.text (words problem)

    BadVersion txt ->
      ["You","provided",P.red (P.text (show txt))
      ,"which","is","not","a","valid","version.","I","need","something","like"
      ,P.green "\"1.0.0\"","or",P.green "\"2.0.4\"" <> "."
      ]

    BadConstraint problem ->
      map P.text (words ("It is not a valid constraint. " ++ problem))

    BadModuleName name ->
      ["You","provided",P.text (show name),"which","is","not","a","valid","module","name."
      ,"I","need","something","like",P.green "\"Html.Events\"","or",P.green "\"Browser.Navigation\"" <> "."
      ]

    BadModuleHeaderTooLong header ->
      ["The",P.text (show header),"header","is","too","long."
      ,"I","need","it","to","be",P.green "under",P.green "20",P.green "characters"
      ,"to","ensure","that","formatting","is","nice","on","the","package","website."
      ]

    BadDependencyName name ->
      ["The",P.text (show name),"entry","is","not","a","valid","package","name."
      ,"I","recommend","deleting","it,","finding","the","package","you","want","on"
      ,"the","package","website,","and","installing","it","with","the"
      ,P.green "`elm install`","command","instead."
      ]

    BadLicense _given suggestions ->
      case suggestions of
        [] ->
          ["I","need","an","OSI","approved","SPDX","license,","like"
          ,P.green "\"BSD-3-Clause\"","or",P.green "\"MIT\".","See"
          ,"<https://spdx.org/licenses/>","for","a","full","list","of","options."
          ]

        code:codes ->
          ["I","need","an","OSI","approved","SPDX","license."]
          ++ ["Maybe you wanted"] ++ oneOf code codes ++ ["instead?"]
          ++ ["See","<https://spdx.org/licenses/>","for","a","full","list","of","options."]

    BadDirectoryNotString ->
      ["I","need","a",P.green "STRING","that","points","to","a","directory","of","Elm","code."
      ]

    BadSummaryTooLong ->
      ["Your","summary","is","too","long.","I","need","it","to"
      ,"be",P.green "under",P.green "80",P.green "characters."
      ]


oneOf :: Text.Text -> [Text.Text] -> [P.Doc]
oneOf code codes =
  let
    toDoc spdx =
      P.green (P.text (show spdx))
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
