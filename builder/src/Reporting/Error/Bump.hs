{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Bump
  ( Error(..)
  , toReport
  )
  where


import Data.Monoid ((<>))
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Elm.Package as Pkg
import qualified Reporting.Error.Help as Help



-- ERRORS


data Error
  = Application
  | Unbumpable Pkg.Version [Pkg.Version]



-- TO REPORT


toReport :: Error -> Help.Report
toReport err =
  case err of
    Application ->
      Help.report "UNBUMPABLE" (Just "elm.json")
        "Your elm.json says this is an application. That means it cannot be published\
        \ on <https://package.elm-lang.org> and therefore has no version to bump!"
        []

    Unbumpable vsn versions ->
      Help.docReport "UNBUMPABLE" (Just "elm.json")
        ( P.fillSep
            ["Your","elm.json","says","I","should","bump","relative","to","version"
            ,P.red (P.text (Pkg.versionToString vsn)) <> ","
            ,"but","I","cannot","find","that","version","on","<https://package.elm-lang.org>."
            ,"That","means","there","is","no","API","for","me","to","diff","against","and"
            ,"figure","out","if","these","are","MAJOR,","MINOR,","or","PATCH","changes."
            ]
        )
        [ P.fillSep $
            ["Try","bumping","again","after","changing","the",P.dullyellow "\"version\"","in","elm.json"]
            ++ if length versions == 1 then ["to:"] else ["to","one","of","these:"]
        , P.vcat $ map (P.green . P.text . Pkg.versionToString) versions
        ]
