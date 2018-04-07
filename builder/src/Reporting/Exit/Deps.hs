{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit.Deps
  ( Exit(..)
  , toReport
  )
  where


import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg
import qualified Elm.Project.Constraint as Con
import Reporting.Doc ((<>))
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help



-- EXITS


data Exit
  = CorruptVersionCache Pkg.Name
  | PackageNotFound Pkg.Name [Pkg.Name]
  -- verify
  | AppBadElm Pkg.Version
  | PkgBadElm Con.Constraint
  | BadDeps
  | BuildFailure Pkg.Name Pkg.Version



-- TO DOC


toReport :: Exit -> Help.Report
toReport exit =
  case exit of
    CorruptVersionCache pkg ->
      Help.report "CORRUPT CACHE" Nothing
        ( "I ran into an unknown package while exploring dependencies:"
        )
        [ D.indent 4 $ D.dullyellow $ D.fromString $ Pkg.toString pkg
        , D.reflow $
            "This suggests that your ELM_HOME directory has been corrupted.\
            \ Maybe some program is messing with it? It is just cached files,\
            \ so you can delete it and see if that fixes the issue."
        ]

    PackageNotFound package suggestions ->
      Help.docReport "PACKAGE NOT FOUND" Nothing
        ( D.fillSep
            ["I","cannot","find","a"
            ,D.red (D.fromString (Pkg.toString package))
            ,"package","on","the","package","website."
            ]
        )
        [ "Maybe you want one of these instead?"
        , D.indent 4 $ D.dullyellow $ D.vcat $ map (D.fromString . Pkg.toString) suggestions
        , "But check <https://package.elm-lang.org> to see all possibilities!"
        ]

    AppBadElm version ->
      Help.report "ELM VERSION MISMATCH" Nothing
        "Your elm.json says this application needs a different version of Elm."
        [ D.fillSep
            [ "It", "requires"
            , D.green (D.fromString (Pkg.versionToString version)) <> ","
            , "but", "you", "are", "using"
            , D.red (D.fromString (Pkg.versionToString Compiler.version))
            , "right", "now."
            ]
        ]

    PkgBadElm constraint ->
      Help.report "ELM VERSION MISMATCH" Nothing
        "Your elm.json says this package needs a version of Elm in this range:"
        [ D.indent 4 $ D.dullyellow $ D.fromString $ Con.toString constraint
        , D.fillSep
            [ "But", "you", "are", "using", "Elm"
            , D.red (D.fromString (Pkg.versionToString Compiler.version))
            , "right", "now."
            ]
        ]

    BadDeps ->
      Help.report "CLASHING PACKAGE DEPENDENCIES" Nothing
        "The dependencies in your elm.json are not compatible."
        [ D.reflow $
            "Did you change them by hand? Try to change it back! It is much\
            \ better to add dependencies with commands like this:"
        , D.indent 4 $ D.dullyellow "elm install elm-lang/http"
        , D.reflow $
            "Please ask for help on the Elm slack <http://elmlang.herokuapp.com/> if\
            \ you are running into something that seems trickier than this."
        ]

    BuildFailure pkg vsn ->
      Help.report "CORRUPT DEPENDENCY" Nothing
        "I ran into a problem while building the following package:"
        [ D.indent 4 $ D.dullyellow $ D.fromString $ Pkg.toString pkg ++ " " ++ Pkg.versionToString vsn
        , D.reflow $
            "This probably means the downloaded files got corrupted somehow.\
            \ Maybe try deleting your ELM_HOME and see if that resolves the issue?"
        ]
