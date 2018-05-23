{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
  where


import Prelude hiding (init)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Data.List as List
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Text.PrettyPrint.ANSI.Leijen ((<>))
import Text.Read (readMaybe)

import qualified Elm.Compiler as Compiler
import qualified Elm.Diff as Diff
import qualified Elm.Package as Pkg
import qualified Generate.Output as Output
import Terminal.Args
import Terminal.Args.Helpers

import qualified Bump
import qualified Develop
import qualified Diff
import qualified Init
import qualified Install
import qualified Make
import qualified Publish
import qualified Repl



-- MAIN


main :: IO ()
main =
  do  setLocaleEncoding utf8
      complex intro outro
        [ repl
        , init
        , reactor
        , make
        , install
        , bump
        , diff
        , publish
        ]


intro :: P.Doc
intro =
  P.vcat
    [ P.fillSep
        ["Hi,","thank","you","for","trying","out"
        ,P.green "Elm"
        ,P.green (P.text (Pkg.versionToString Compiler.version)) <> "."
        ,"I hope you like it!"
        ]
    , ""
    , P.black "-------------------------------------------------------------------------------"
    , P.black "I highly recommend working through <https://guide.elm-lang.org> to get started."
    , P.black "It teaches many important concepts, including how to use `elm` in the terminal."
    , P.black "-------------------------------------------------------------------------------"
    ]


outro :: P.Doc
outro =
  P.fillSep $ map P.text $ words $
    "Be sure to ask on the Elm slack if you run into trouble! Folks are friendly and\
    \ happy to help out. They hang out there because it is fun, so be kind to get the\
    \ best results!"



-- INIT


init :: Interface
init =
  let
    summary =
      "Start an Elm project. It creates two files (elm.json and src/Main.elm)\
      \ and provides a couple links that explain how to get going with Elm."

    details =
      "The `init` command helps start Elm projects:"

    example =
      reflow
        "It asks permission to create two files (elm.json and src/Main.elm) that are\
        \ common to all Elm projects. It also provides a couple links that explain what\
        \ to do from there."

    initFlags =
      flags Init.Flags
        |-- onOff "embed" "Switch Browser.sandbox to Browser.embed, making it easier to jump into making HTTP requests, generating random values, asking the time, etc."
        |-- onOff "fullscreen" "Switch Browser.sandbox to Browser.fullscreen, setting you up for a single-page app that controls the <title> and URL of the browser."
  in
  Interface "init" (Common summary) details example noArgs initFlags Init.run



-- REPL


repl :: Interface
repl =
  let
    summary =
      "Open up an interactive programming session. Type in Elm expressions\
      \ like (2 + 2) or (String.length \"test\") and see if they equal four!"

    details =
      "The `repl` command opens up an interactive programming session:"

    example =
      reflow
        "Start working through <https://guide.elm-lang.org> to learn how to use this!\
        \ It has a whole chapter that uses the REPL for everything, so that is probably\
        \ the quickest way to get started."

    replFlags =
      flags Repl.Flags
        |-- flag "interpreter" interpreter "Path to a alternate JS interpreter, like node or nodejs."
        |-- onOff "no-colors" "Turn off the colors in the REPL. This can help if you are having trouble reading the values. Some terminals use a custom color scheme that diverges significantly from the standard ANSI colors, so another path may be to pick a more standard color scheme."
  in
  Interface "repl" (Common summary) details example noArgs replFlags Repl.run


interpreter :: Parser String
interpreter =
  Parser
    { _singular = "interpreter"
    , _plural = "interpreters"
    , _parser = Just
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["node","nodejs"]
    }



-- REACTOR


reactor :: Interface
reactor =
  let
    summary =
      "Compile code with a click. It opens a file viewer in your browser, and\
      \ when you click on an Elm file, it compiles and you see the result."

    details =
      "The `reactor` command starts a local server on your computer:"

    example =
      reflow
        "After running that command, you would have a server at <http://localhost:8000>\
        \ that helps with development. It shows your files like a file viewer. If you\
        \ click on an Elm file, it will compile it for you! And you can just press\
        \ the refresh button in the browser to recompile things."

    reactorFlags =
      flags Develop.Flags
        |-- flag "port" port_ "The port of the server (default: 8000)"
  in
  Interface "reactor" (Common summary) details example noArgs reactorFlags Develop.run


port_ :: Parser Int
port_ =
  Parser
    { _singular = "port"
    , _plural = "ports"
    , _parser = readMaybe
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["3000","8000"]
    }



-- MAKE


make :: Interface
make =
  let
    details =
      "The `make` command compiles Elm code into JS or HTML:"

    example =
      stack
        [ reflow
            "For example:"
        , P.indent 4 $ P.green "elm make src/Main.elm"
        , reflow
            "This tries to compile an Elm file named src/Main.elm, putting the resulting\
            \ JavaScript code in an elm.js file."
        ]

    makeFlags =
      flags Make.Flags
        |-- onOff "debug" "Turn on the time-travelling debugger. It allows you to rewind and replay events. The events can be imported/exported into a file, which makes for very precise bug reports!"
        |-- onOff "optimize" "Turn on optimizations to make code smaller and faster. For example, the compiler renames record fields to be as short as possible and unboxes values to reduce allocation."
        |-- flag "output" Output.output "Specify the name of the resulting JS file. For example --output=assets/elm.js to generate the JS at assets/elm.js or --output=/dev/null to generate no output at all!"
        |-- flag "report" Make.reportType "You can say --report=json to get error messages as JSON. This is only really useful if you are an editor plugin. Humans should avoid it!"
        |-- flag "docs" Make.docsFile "Generate a JSON file of documentation for a package. Eventually it will be possible to preview docs with `reactor` because it is quite hard to deal with these JSON files directly."
  in
  Interface "make" Uncommon details example (zeroOrMore elmFile) makeFlags Make.run



-- INSTALL


install :: Interface
install =
  let
    details =
      "The `install` command fetches packages from <https://package.elm-lang.org> for\
      \ use in your project:"

    example =
      stack
        [ reflow
            "For example, if you want to get packages for HTTP and JSON, you would say:"
        , P.indent 4 $ P.green $ P.vcat $
              [ "elm install elm/http"
              , "elm install elm/json"
              ]
        , reflow
            "Notice that you must say the AUTHOR name and PROJECT name! After running those\
            \ commands, you could say `import Http` or `import Json.Decode` in your code."
        , reflow
            "What if two projects use different versions of the same package? No problem!\
            \ Each project is independent, so there cannot be conflics like that!"
        ]

    installArgs =
      oneOf
        [ require0 Install.NoArgs
        , require1 Install.Install package
        ]
  in
  Interface "install" Uncommon details example installArgs noFlags Install.run



-- PUBLISH


publish :: Interface
publish =
  let
    details =
      "The `publish` command publishes your package on <https://package.elm-lang.org>\
      \ so that anyone in the Elm community can use it."

    example =
      stack
        [ reflow
            "Think hard if you are ready to publish NEW packages though!"
        , reflow
            "Part of what makes Elm great is the packages ecosystem. The fact that\
            \ there is usually one option (usually very well done) makes it way\
            \ easier to pick packages and become productive. So having a million\
            \ packages would be a failure in Elm. We do not need twenty of\
            \ everything, all coded in a single weekend."
        , reflow
            "So as community members gain wisdom through experience, we want\
            \ them to share that through thoughtful API design and excellent\
            \ documentation. It is more about sharing ideas and insights than\
            \ just sharing code! The first step may be asking for advice from\
            \ people you respect, or in community forums. The second step may\
            \ be using it at work to see if it is as nice as you think. Maybe\
            \ it ends up as an experiment on GitHub only. Point is, try to be\
            \ respectful of the community and package ecosystem!"
        , reflow
            "Check out <TODO> for guidance on how to create great packages!"
        ]
  in
  Interface "publish" Uncommon details example noArgs noFlags Publish.run



-- BUMP


bump :: Interface
bump =
  let
    details =
      "The `bump` command figures out the next version number based on API changes:"

    example =
      reflow
        "Say you just published version 1.0.0, but then decided to remove a function.\
        \ I will compare the published API to what you have locally, figure out that\
        \ it is a MAJOR change, and bump your version number to 2.0.0. I do this with\
        \ all packages, so there cannot be MAJOR changes hiding in PATCH releases in Elm!"
  in
  Interface "bump" Uncommon details example noArgs noFlags Bump.run



-- DIFF


diff :: Interface
diff =
  let
    details =
      "The `diff` command detects API changes:"

    example =
      stack
        [ reflow
            "For example, to see what changed in the HTML package between\
            \ versions 1.0.0 and 2.0.0, you can say:"
        , P.indent 4 $ P.green $ "elm diff elm/html 1.0.0 2.0.0"
        , reflow
            "Sometimes a MAJOR change is not actually very big, so\
            \ this can help you plan your upgrade timelines."
        ]

    diffArgs =
      oneOf
        [ require0 Diff.CodeVsLatest
        , require1 Diff.CodeVsExactly version
        , require2 Diff.LocalInquiry version version
        , require3 Diff.GlobalInquiry package version version
        ]
  in
  Interface "diff" Uncommon details example diffArgs noFlags Diff.run



-- HELPERS


stack :: [P.Doc] -> P.Doc
stack docs =
  P.vcat $ List.intersperse "" docs


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string
