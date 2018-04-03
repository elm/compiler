{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit
  ( Exit(..)
  , toString
  , toStderr
  , toJson
  )
  where


import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import System.IO (stderr)
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg
import qualified Elm.Utils as Utils
import qualified Json.Encode as Encode
import qualified Reporting.Exit.Assets as Asset
import qualified Reporting.Exit.Bump as Bump
import qualified Reporting.Exit.Compile as Compile
import qualified Reporting.Exit.Crawl as Crawl
import qualified Reporting.Exit.Deps as Deps
import qualified Reporting.Exit.Diff as Diff
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Exit.Http as Http
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
  | Publish Publish.Exit
  | BadHttp String Http.Exit

  -- misc
  | NoSolution [Pkg.Name]
  | CannotMakeNothing
  | CannotOptimizeDebug Module.Raw [Module.Raw]



-- RENDERERS


toString :: Exit -> String
toString exit =
  Help.toString (Help.reportToDoc (toReport exit))


toStderr :: Exit -> IO ()
toStderr exit =
  Help.toStderr (Help.reportToDoc (toReport exit))


toJson :: Exit -> IO ()
toJson exit =
  B.hPutBuilder stderr $ Encode.encodeUgly $ Help.reportToJson (toReport exit)


toReport :: Exit -> Help.Report
toReport exit =
  case exit of
    NoElmJson ->
      Help.report "WELCOME" Nothing
        "It looks like you are trying to start a new Elm project. Very exciting! :D"
        [ P.fillSep
            ["I","very","highly","recommend","working","through"
            ,P.green "<https://guide.elm-lang.org>","which","will","teach","you","the"
            ,"basics","of","Elm,","including","how","to","start","new","projects."
            ]
        , P.fillSep
            ["For","folks","who","have","already","built","stuff","with","Elm,","the"
            ,"problem","is","just","that","there","is","no",P.dullyellow "elm.json","yet."
            ,"If","you","want","to","work","from","an","example,","check","out","the"
            ,"one","at","<https://github.com/evancz/elm-todomvc/blob/master/elm.json>"
            ]
        , Help.reflow
            "Whatever your scenario, I hope you have a lovely time using Elm!"
        ]

    Assets assetError ->
      Asset.toReport assetError

    Bump bumpError ->
      Bump.toReport bumpError

    Compile e es ->
      Help.compilerReport e es

    Crawl crawlError ->
      Crawl.toReport crawlError

    Cycle names ->
      Help.report "IMPORT CYCLE" Nothing
        "Your module imports form a cycle:"
        [ P.indent 4 (Utils.drawCycle names)
        , Help.reflow $
            "Learn more about why this is disallowed and how to break cycles here:"
            ++ Help.hintLink "import-cycles"
        ]

    Deps depsError ->
      Deps.toReport depsError

    Diff commandsError ->
      Diff.toReport commandsError

    Publish publishError ->
      Publish.toReport publishError

    BadHttp url httpError ->
      Http.toReport url httpError

    NoSolution badPackages ->
      case badPackages of
        [] ->
          Help.report "UNSOLVABLE DEPENDENCIES" (Just "elm.json")
            "This usually happens if you try to modify dependency constraints by\
            \ hand. I recommend deleting any dependency you added recently (or all\
            \ of them if things are bad) and then adding them again with:"
            [ P.indent 4 $ P.green "elm install"
            , Help.reflow $
                "And do not be afaid to ask for help on Slack if you get stuck!"
            ]

        _:_ ->
          Help.report "OLD DEPENDENCIES" (Just "elm.json")
            ( "The following packages do not work with Elm " ++ Pkg.versionToString Compiler.version ++ " right now:"
            )
            [ P.indent 4 $ P.vcat $ map (P.red . P.text . Pkg.toString) badPackages
            , Help.reflow $
                "This may be because it is not upgraded yet. It may be because a\
                \ better solution came along, so there was no need to upgrade it.\
                \ Etc. Try asking around on Slack to learn more about the topic."
            , Help.note
                "Whatever the case, please be kind to the relevant package authors! Having\
                \ friendly interactions with users is great motivation, and conversely, getting\
                \ berated by strangers on the internet sucks your soul dry. Furthermore, package\
                \ authors are humans with families, friends, jobs, vacations, responsibilities,\
                \ goals, etc. They face obstacles outside of their technical work you will never\
                \ know about, so please assume the best and try to be patient and supportive!"
            ]

    CannotMakeNothing ->
      Help.report "NO INPUT" Nothing
        "What should I make though? I need more information, like:"
        [ P.vcat
            [ P.indent 4 $ P.green "elm make MyThing.elm"
            , P.indent 4 $ P.green "elm make This.elm That.elm"
            ]
        , Help.reflow
            "However many files you give, I will create one JS file out of them."
        ]

    CannotOptimizeDebug m ms ->
      Help.report "DEBUG REMNANTS" Nothing
      "There are uses of the `Debug` module in the following modules:"
        [ P.indent 4 $ P.red $ P.vcat $ map (P.text . Module.nameToString) (m:ms)
        , Help.reflow "But the --optimize flag only works if all `Debug` functions are removed!"
        , toSimpleNote $
            "The issue is that --optimize strips out info needed by `Debug` functions.\
            \ Here are two examples:"
        , P.indent 4 $ Help.reflow $
            "(1) It shortens record field names. This makes the generated JavaScript is\
            \ smaller, but `Debug.toString` cannot know the real field names anymore."
        , P.indent 4 $ Help.reflow $
            "(2) Values like `type Height = Height Float` are unboxed. This reduces\
            \ allocation, but it also means that `Debug.toString` cannot tell if it is\
            \ looking at a `Height` or `Float` value."
        , Help.reflow $
            "There are a few other cases like that, and it will be much worse once we start\
            \ inlining code. That optimization could move `Debug.log` and `Debug.todo` calls,\
            \ resulting in unpredictable behavior. I hope that clarifies why this restriction\
            \ exists!"
        ]


toSimpleNote :: String -> P.Doc
toSimpleNote message =
  P.fillSep ((P.underline "Note" <> ":") : map P.text (words message))
