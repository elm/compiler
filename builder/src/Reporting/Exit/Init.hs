{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit.Init
  ( Exit(..)
  , toReport
  )
  where


import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help

import qualified Elm.Package as Pkg



-- EXITS


data Exit
  = NoSolution [Pkg.Name]
  | AlreadyStarted



-- TO REPORT


toReport :: Exit -> Help.Report
toReport exit =
  case exit of
    NoSolution pkgs ->
      Help.report "NO SOLUTION" Nothing
        "I tried to create an elm.json with the following direct dependencies:"
        [ D.indent 4 $ D.vcat $
            map (D.dullyellow . D.fromString . Pkg.toString) pkgs
        , D.reflow $
            "I could not find compatible versions though! This should not happen, so please\
            \ ask around one of the community forums <http://elm-lang.org/community> to learn\
            \ what is going on!"
        ]

    AlreadyStarted ->
      Help.report "EXISTING PROJECT" Nothing
        "You already have an elm.json file, so there is nothing for me to initialize!"
        [ D.fillSep
            ["Maybe",D.green (D.fromString (D.makeLink "init")),"can","help"
            ,"you","figure","out","what","to","do","next?"
            ]
        ]
