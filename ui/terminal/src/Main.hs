{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
  where


import GHC.IO.Encoding (setLocaleEncoding, utf8)
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



-- REPL


repl :: Interface
repl =
  let
    summary =
      "Open up an interactive programming session. You can type in Elm\
      \ expressions and it will tell you what they evaluate to."

    details =
      Details $ error "TODO"

    replFlags =
      flags Repl.Flags
        |-- flag "interpreter" interpreter "Path to a alternate JS interpreter, like node or nodejs."
  in
  Interface "repl" (Common summary) details noArgs replFlags Repl.run


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
      "Easy way to get started with Elm. It shows a file viewer, and when you\
      \ click on an Elm file, it compiles it and shows the result."

    details =
      Details $ error "TODO"

    reactorFlags =
      flags Develop.Flags
        |-- flag "port" port_ "The port of the server (default: 8000)"
  in
  Interface "reactor" (Common summary) details noArgs reactorFlags Develop.run


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
    summary =
      "Compile files by hand. Once you get comfortable with Elm projects, you\
      \ will probably want this level of control during development."

    details =
      Details $ error "TODO"

    makeFlags =
      flags Make.Flags
        |-- onOff "warn" "Report warnings to improve code quality."
        |-- onOff "debug" "Generate programs in debug mode."
        |-- flag "output" Output.output "Specify the name of the resulting JS file (use --output=/dev/null for no output)"
  in
  Interface "make" (Common summary) details (zeroOrMore elmFile) makeFlags Make.run



-- INSTALL


install :: Interface
install =
  let
    summary =
      "Install <https://package.elm-lang.org> packages. Every project is\
      \ independent, so you can use different versions in different projects."

    details =
      Details $ error "TODO"
  in
  Interface "install" (Common summary) details (required package) noFlags Install.run



-- PUBLISH


publish :: Interface
publish =
  let
    details =
      Details $ error "TODO"
  in
  Interface "publish" Uncommon details noArgs noFlags Publish.run



-- BUMP


bump :: Interface
bump =
  let
    details =
      Details $ error "TODO"
  in
  Interface "bump" Uncommon details noArgs noFlags Bump.run



-- DIFF


diff :: Interface
diff =
  let
    details =
      Details $
        "The `diff` command detects API changes. Every Elm package has a known public\
        \ API, so I can tell you exactly what changed from version to version."

    diffArgs =
      oneOf
        [ require0 Diff.CodeVsLatest
        , require1 Diff.CodeVsExactly version
        , require2 Diff.LocalInquiry version version
        , require3 Diff.GlobalInquiry package version version
        ]
  in
  Interface "diff" Uncommon details diffArgs noFlags Diff.run
