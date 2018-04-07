{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit.Bump
  ( Exit(..)
  , toReport
  )
  where


import qualified Elm.Package as Pkg
import Reporting.Doc ((<>))
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help



-- EXITS


data Exit
  = Application
  | Unbumpable Pkg.Version [Pkg.Version]



-- TO REPORT


toReport :: Exit -> Help.Report
toReport exit =
  case exit of
    Application ->
      Help.report "UNBUMPABLE" (Just "elm.json")
        "Your elm.json says this is an application. That means it cannot be published\
        \ on <https://package.elm-lang.org> and therefore has no version to bump!"
        []

    Unbumpable vsn versions ->
      Help.docReport "UNBUMPABLE" (Just "elm.json")
        ( D.fillSep
            ["Your","elm.json","says","I","should","bump","relative","to","version"
            ,D.red (D.fromText (Pkg.versionToText vsn)) <> ","
            ,"but","I","cannot","find","that","version","on","<https://package.elm-lang.org>."
            ,"That","means","there","is","no","API","for","me","to","diff","against","and"
            ,"figure","out","if","these","are","MAJOR,","MINOR,","or","PATCH","changes."
            ]
        )
        [ D.fillSep $
            ["Try","bumping","again","after","changing","the",D.dullyellow "\"version\"","in","elm.json"]
            ++ if length versions == 1 then ["to:"] else ["to","one","of","these:"]
        , D.vcat $ map (D.green . D.fromText . Pkg.versionToText) versions
        ]
