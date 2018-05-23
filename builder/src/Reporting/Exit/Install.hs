{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit.Install
  ( Exit(..)
  , toReport
  )
  where


import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help



-- EXITS


data Exit
  = NoArgs FilePath
  | NoSolution [Pkg.Name]



-- TO REPORT


toReport :: Exit -> Help.Report
toReport exit =
  case exit of
    NoArgs elmHome ->
      Help.report "INSTALL WHAT?" Nothing
        "I am expecting commands like:"
        [ D.green $ D.indent 4 $ D.vcat $
            [ "elm install elm/http"
            , "elm install elm/json"
            , "elm install elm/random"
            ]
        , D.toFancyHint
            ["In","JavaScript","folks","run","`npm install`","to","start","projects."
            ,"\"Gotta","download","everything!\"","But","why","download","packages"
            ,"again","and","again?","Instead,","Elm","caches","packages","in"
            ,D.dullyellow (D.fromString elmHome)
            ,"so","each","one","is","downloaded","and","built","ONCE","on","your","machine."
            ,"Elm","projects","check","that","cache","before","trying","the","internet."
            ,"This","reduces","build","times,","reduces","server","costs,","and","makes","it"
            ,"easier","to","work","offline.","As","a","result"
            ,D.dullcyan "elm install","is","only","for","adding","dependencies","to","elm.json,"
            ,"whereas",D.dullcyan "elm make","is","in","charge","of","gathering","dependencies"
            ,"and","building","everything.","So","maybe","try",D.green "elm make","instead?"
            ]
        ]

    NoSolution badPackages ->
      case badPackages of
        [] ->
          Help.report "UNSOLVABLE DEPENDENCIES" (Just "elm.json")
            "This usually happens if you try to modify dependency constraints by\
            \ hand. I recommend deleting any dependency you added recently (or all\
            \ of them if things are bad) and then adding them again with:"
            [ D.indent 4 $ D.green "elm install"
            , D.reflow $
                "And do not be afaid to ask for help on Slack if you get stuck!"
            ]

        _:_ ->
          Help.report "OLD DEPENDENCIES" (Just "elm.json")
            ( "The following packages do not work with Elm " ++ Pkg.versionToString Compiler.version ++ " right now:"
            )
            [ D.indent 4 $ D.vcat $ map (D.red . D.fromString . Pkg.toString) badPackages
            , D.reflow $
                "This may be because it is not upgraded yet. It may be because a\
                \ better solution came along, so there was no need to upgrade it.\
                \ Etc. Try asking around on Slack to learn more about the topic."
            , D.toSimpleNote
                "Whatever the case, please be kind to the relevant package authors! Having\
                \ friendly interactions with users is great motivation, and conversely, getting\
                \ berated by strangers on the internet sucks your soul dry. Furthermore, package\
                \ authors are humans with families, friends, jobs, vacations, responsibilities,\
                \ goals, etc. They face obstacles outside of their technical work you will never\
                \ know about, so please assume the best and try to be patient and supportive!"
            ]
