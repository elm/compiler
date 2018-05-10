{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit
  ( Exit(..)
  , toString
  , toStderr
  , toJson
  )
  where


import qualified Elm.Compiler.Module as Module
import qualified Json.Encode as Encode
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Assets as Asset
import qualified Reporting.Exit.Bump as Bump
import qualified Reporting.Exit.Compile as Compile
import qualified Reporting.Exit.Crawl as Crawl
import qualified Reporting.Exit.Deps as Deps
import qualified Reporting.Exit.Diff as Diff
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Exit.Http as Http
import qualified Reporting.Exit.Make as Make
import qualified Reporting.Exit.Install as Install
import qualified Reporting.Exit.Publish as Publish



-- ALL POSSIBLE ERRORS


data Exit
  = NoElmJson
  | Assets Asset.Exit
  | Bump Bump.Exit
  | Compile Compile.Exit [Compile.Exit]
  | Crawl Crawl.Exit
  | Cycle [Module.Raw] -- TODO write docs to help with this scenario
  | Deps Deps.Exit
  | Diff Diff.Exit
  | Make Make.Exit
  | Install Install.Exit
  | Publish Publish.Exit
  | BadHttp String Http.Exit



-- RENDERERS


toString :: Exit -> String
toString exit =
  Help.toString (Help.reportToDoc (toReport exit))


toStderr :: Exit -> IO ()
toStderr exit =
  Help.toStderr (Help.reportToDoc (toReport exit))


toJson :: Exit -> Encode.Value
toJson exit =
  Help.reportToJson (toReport exit)


toReport :: Exit -> Help.Report
toReport exit =
  case exit of
    NoElmJson ->
      Help.report "WELCOME" Nothing
        "It looks like you are trying to start a new Elm project. Very exciting! :D"
        [ D.fillSep
            ["I","very","highly","recommend","working","through"
            ,D.green "<https://guide.elm-lang.org>","which","will","teach","you","the"
            ,"basics","of","Elm,","including","how","to","start","new","projects."
            ]
        , D.fillSep
            ["For","folks","who","have","already","built","stuff","with","Elm,","the"
            ,"problem","is","just","that","there","is","no",D.dullyellow "elm.json","yet."
            ,"If","you","want","to","work","from","an","example,","check","out","the"
            ,"one","at","<https://github.com/evancz/elm-todomvc/blob/master/elm.json>"
            ]
        , D.reflow
            "Whatever your scenario, I hope you have a lovely time using Elm!"
        ]

    Assets assetExit ->
      Asset.toReport assetExit

    Bump bumpExit ->
      Bump.toReport bumpExit

    Compile e es ->
      Help.compilerReport e es

    Crawl crawlExit ->
      Crawl.toReport crawlExit

    Cycle names ->
      Help.report "IMPORT CYCLE" Nothing
        "Your module imports form a cycle:"
        [ D.cycle 4 names
        , D.reflow $
            "Learn more about why this is disallowed and how to break cycles here:"
            ++ D.makeLink "import-cycles"
        ]

    Deps depsExit ->
      Deps.toReport depsExit

    Diff commandsExit ->
      Diff.toReport commandsExit

    Make makeExit ->
      Make.toReport makeExit

    Install installExit ->
      Install.toReport installExit

    Publish publishExit ->
      Publish.toReport publishExit

    BadHttp url httpExit ->
      Http.toReport url httpExit
