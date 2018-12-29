{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit.Compile
  ( Exit(..)
  , toJson
  , toDoc
  )
  where


import qualified Data.List as List
import qualified Data.Time.Clock as Time

import qualified Elm.Compiler as Compiler
import qualified Elm.ModuleName as ModuleName
import qualified Json.Encode as Encode
import qualified Reporting.Doc as D



-- EXITS


data Exit =
  Exit
    { _name :: ModuleName.Raw
    , _path :: FilePath
    , _time :: Time.UTCTime
    , _source :: String
    , _errors :: [Compiler.Error]
    }



-- TO JSON


toJson :: Exit -> Encode.Value
toJson (Exit name path _ source errors) =
  Compiler.errorsToJson name path source errors



-- TO DOC


toDoc :: Exit -> [Exit] -> D.Doc
toDoc e es =
  let
    (exit, exits) = sortByTime e es
  in
  D.vcat (toDocHelp exit exits)


toDocHelp :: Exit -> [Exit] -> [D.Doc]
toDocHelp e1 exits =
  case exits of
    [] ->
      [exitToDoc e1]

    e2 : otherErrors ->
      exitToDoc e1 : separator (_name e1) (_name e2) : toDocHelp e2 otherErrors


exitToDoc :: Exit -> D.Doc
exitToDoc (Exit _name path _time source errors) =
  Compiler.errorsToDoc path source errors


separator :: ModuleName.Raw -> ModuleName.Raw -> D.Doc
separator beforeName afterName =
  let
    before = ModuleName.nameToChars beforeName ++ "  ↑    "
    after  = "    ↓  " ++  ModuleName.nameToChars afterName
  in
    D.dullred $ D.vcat $
      [ D.indent (80 - length before) (D.fromChars before)
      , "====o======================================================================o===="
      , D.fromChars after
      , D.empty
      , D.empty
      ]



-- SORT BY TIME


sortByTime :: Exit -> [Exit] -> (Exit, [Exit])
sortByTime exit exits =
  case List.sortBy timeCompare exits of
    [] ->
      (exit, [])

    e:es ->
      if _time exit < _time e then
        (exit, e:es)
      else
        (e, List.insertBy timeCompare exit es)


timeCompare :: Exit -> Exit -> Ordering
timeCompare e1 e2 =
  compare (_time e1) (_time e2)
