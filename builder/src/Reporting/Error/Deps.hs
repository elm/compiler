{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Deps
  ( Error(..)
  , toReport
  )
  where

import Text.PrettyPrint.ANSI.Leijen ((<>))
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg
import qualified Elm.Project.Constraint as Con
import qualified Reporting.Error.Help as Help



-- ERRORS


data Error
  = CorruptVersionCache Pkg.Name
  | PackageNotFound Pkg.Name [Pkg.Name]
  -- verify
  | AppBadElm Pkg.Version
  | PkgBadElm Con.Constraint
  | BadDeps
  | BuildFailure Pkg.Name Pkg.Version



-- TO DOC


toReport :: Error -> Help.Report
toReport err =
  case err of
    CorruptVersionCache pkg ->
      Help.report "CORRUPT CACHE" Nothing
        ( "I ran into an unknown package while exploring dependencies:"
        )
        [ P.indent 4 $ P.dullyellow $ P.text $ Pkg.toString pkg
        , Help.reflow $
            "This suggests that your ELM_HOME directory has been corrupted.\
            \ Maybe some program is messing with it? It is just cached files,\
            \ so you can delete it and see if that fixes the issue."
        ]

    PackageNotFound package suggestions ->
      Help.report "PACKAGE NOT FOUND" Nothing
        ( "Could not find any packages named " ++ Pkg.toString package
        )
        [ P.text $ "Maybe you want one of these instead?"
        , P.indent 4 $ P.vcat $ map (P.text . Pkg.toString) suggestions
        ]

    AppBadElm version ->
      Help.report "ELM VERSION MISMATCH" Nothing
        "Your elm.json says this application needs a different version of Elm."
        [ P.fillSep
            [ "It", "requires"
            , P.green (P.text (Pkg.versionToString version)) <> ","
            , "but", "you", "are", "using"
            , P.red (P.text (Pkg.versionToString Compiler.version))
            , "right", "now."
            ]
        ]

    PkgBadElm constraint ->
      Help.report "ELM VERSION MISMATCH" Nothing
        "Your elm.json says this package needs a version of Elm in this range:"
        [ P.indent 4 $ P.dullyellow $ P.text $ Con.toString constraint
        , P.fillSep
            [ "But", "you", "are", "using", "Elm"
            , P.red (P.text (Pkg.versionToString Compiler.version))
            , "right", "now."
            ]
        ]

    BadDeps ->
      Help.report "CLASHING PACKAGE DEPENDENCIES" Nothing
        "The dependencies in your elm.json are not compatible."
        [ Help.reflow $
            "Did you change them by hand? Try to change it back! It is much\
            \ better to add dependencies with commands like this:"
        , P.indent 4 $ P.dullyellow $ P.text $ "elm install elm-lang/http"
        , Help.reflow $
            "Please ask for help on the Elm slack <http://elmlang.herokuapp.com/> if\
            \ you are running into something that seems trickier than this."
        ]

    BuildFailure pkg vsn ->
      Help.report "CORRUPT DEPENDENCY" Nothing
        "I ran into a problem while building the following package:"
        [ P.indent 4 $ P.dullyellow $ P.text $ Pkg.toString pkg ++ " " ++ Pkg.versionToString vsn
        , Help.reflow $
            "This probably means the downloaded files got corrupted somehow.\
            \ Maybe try deleting your ELM_HOME and see if that resolves the issue?"
        ]
