{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit
  ( Exit(..)
  , toString
  , toStderr
  , toJson
  )
  where


import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
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
  | Publish Publish.Exit
  | BadHttp String Http.Exit

  -- misc
  | NoSolution [Pkg.Name]



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

    Publish publishExit ->
      Publish.toReport publishExit

    BadHttp url httpExit ->
      Http.toReport url httpExit

    NoSolution badPackages ->
      case badPackages of
        [] ->
          Help.report "UNSOLVABLE DEPENDENCIES" (Just "elm.json")
            "This usually happens if you try to modify dependency constraints by\
            \ hand. I recommend deleting any dependency you added recently (or all\
            \ of them if things are bad) and then adding them again with:"
            [ D.indent 4 $ D.green "elm install"
            , D.reflow $
                "And do not be afaid to ask for help on Slack if you get stuck!"
            ]

        _:_ ->
          Help.report "OLD DEPENDENCIES" (Just "elm.json")
            ( "The following packages do not work with Elm " ++ Pkg.versionToString Compiler.version ++ " right now:"
            )
            [ D.indent 4 $ D.vcat $ map (D.red . D.fromString . Pkg.toString) badPackages
            , D.reflow $
                "This may be because it is not upgraded yet. It may be because a\
                \ better solution came along, so there was no need to upgrade it.\
                \ Etc. Try asking around on Slack to learn more about the topic."
            , D.toSimpleNote
                "Whatever the case, please be kind to the relevant package authors! Having\
                \ friendly interactions with users is great motivation, and conversely, getting\
                \ berated by strangers on the internet sucks your soul dry. Furthermore, package\
                \ authors are humans with families, friends, jobs, vacations, responsibilities,\
                \ goals, etc. They face obstacles outside of their technical work you will never\
                \ know about, so please assume the best and try to be patient and supportive!"
            ]
