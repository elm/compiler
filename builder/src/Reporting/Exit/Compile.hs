{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit.Compile
  ( Exit(..)
  , toDoc
  )
  where


import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Time.Clock as Time
import qualified Text.PrettyPrint.ANSI.Leijen as P

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module



-- EXITS


data Exit =
  Exit
    { _name :: Module.Raw
    , _path :: FilePath
    , _time :: Time.UTCTime
    , _source :: Text.Text
    , _errors :: [Compiler.Error]
    }



-- TO DOC


toDoc :: Exit -> [Exit] -> P.Doc
toDoc e es =
  case NonEmpty.sortWith _time (e :| es) of
    exit :| exits ->
      P.vcat (toDocHelp exit exits)


toDocHelp :: Exit -> [Exit] -> [P.Doc]
toDocHelp e1 exits =
  case exits of
    [] ->
      [exitToDoc e1]

    e2 : otherErrors ->
      exitToDoc e1 : separator (_name e1) (_name e2) : toDocHelp e2 otherErrors


exitToDoc :: Exit -> P.Doc
exitToDoc (Exit _name path _time source errors) =
  Compiler.errorsToDoc path source errors


separator :: Module.Raw -> Module.Raw -> P.Doc
separator beforeName afterName =
  let
    before = Module.nameToString beforeName ++ "  ↑    "
    after  = "    ↓  " ++  Module.nameToString afterName
  in
    P.dullred $ P.vcat $
      [ P.indent (80 - length before) (P.text before)
      , "====o======================================================================o===="
      , P.text after
      , P.empty
      , P.empty
      ]
