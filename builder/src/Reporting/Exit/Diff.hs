{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit.Diff
  ( Exit(..)
  , toReport
  )
  where

import qualified Data.List as List
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Elm.Package as Pkg
import qualified Reporting.Exit.Help as Help



-- EXITS


data Exit
  = Application
  | UnknownPackage Pkg.Name [Pkg.Name]
  | UnknownVersion Pkg.Name Pkg.Version [Pkg.Version]



-- TO DOC


toReport :: Exit -> Help.Report
toReport exit =
  case exit of
    Application ->
      Help.report "CANNOT DIFF APPLICATIONS" Nothing
        "I cannot perform diffs on applications, only packages! If you are\
        \ just curious to see a diff, try running this command:"
        [ P.indent 4 $ P.green $ "elm diff elm-lang/html 5.1.1 6.0.0"
        ]

    UnknownPackage pkg suggestions ->
      Help.report "UNKNOWN PACKAGE" Nothing
        ( "I cannot find a package called:"
        )
        [ P.indent 4 $ P.red $ P.text $ Pkg.toString pkg
        , "Maybe you want one of these instead?"
        , P.indent 4 $ P.dullyellow $ P.vcat $ map (P.text . Pkg.toString) suggestions
        , "But check <https://package.elm-lang.org> to see all possibilities!"
        ]

    UnknownVersion _pkg vsn realVersions ->
      Help.docReport "UNKNOWN VERSION" Nothing
        ( P.fillSep $
            [ "Version", P.red (P.text (Pkg.versionToString vsn))
            , "has", "never", "been", "published,", "so", "I"
            , "cannot", "diff", "against", "it."
            ]
        )
        [ "Here are all the versions that HAVE been published:"
        , P.indent 4 $ P.dullyellow $ P.vcat $
            let
              sameMajor v1 v2 = Pkg._major v1 == Pkg._major v2
              mkRow vsns = P.hsep $ map (P.text . Pkg.versionToString) vsns
            in
              map mkRow $ List.groupBy sameMajor (List.sort realVersions)
        , "Want one of those instead?"
        ]
