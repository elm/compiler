{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit.Diff
  ( Exit(..)
  , toReport
  )
  where


import qualified Data.List as List

import qualified Elm.Package as Pkg
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help



-- EXITS


data Exit
  = Application
  | Unpublished
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
        [ D.indent 4 $ D.green $ "elm diff elm/html 5.1.1 6.0.0"
        ]

    Unpublished ->
      Help.report "UNPUBLISHED" Nothing
        "This package is not published yet. There is nothing to diff against!"
        []

    UnknownPackage pkg suggestions ->
      Help.report "UNKNOWN PACKAGE" Nothing
        ( "I cannot find a package called:"
        )
        [ D.indent 4 $ D.red $ D.fromText $ Pkg.toText pkg
        , "Maybe you want one of these instead?"
        , D.indent 4 $ D.dullyellow $ D.vcat $ map (D.fromText . Pkg.toText) suggestions
        , "But check <https://package.elm-lang.org> to see all possibilities!"
        ]

    UnknownVersion _pkg vsn realVersions ->
      Help.docReport "UNKNOWN VERSION" Nothing
        ( D.fillSep $
            [ "Version", D.red (D.fromText (Pkg.versionToText vsn))
            , "has", "never", "been", "published,", "so", "I"
            , "cannot", "diff", "against", "it."
            ]
        )
        [ "Here are all the versions that HAVE been published:"
        , D.indent 4 $ D.dullyellow $ D.vcat $
            let
              sameMajor v1 v2 = Pkg._major v1 == Pkg._major v2
              mkRow vsns = D.hsep $ map (D.fromText . Pkg.versionToText) vsns
            in
              map mkRow $ List.groupBy sameMajor (List.sort realVersions)
        , "Want one of those instead?"
        ]
