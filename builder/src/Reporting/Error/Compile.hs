{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Compile
  ( Error(..)
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



-- ERRORS


data Error =
  Error
    { _name :: Module.Raw
    , _path :: FilePath
    , _time :: Time.UTCTime
    , _source :: Text.Text
    , _errors :: [Compiler.Error]
    }



-- TO DOC


toDoc :: Error -> [Error] -> P.Doc
toDoc e es =
  case NonEmpty.sortWith _time (e :| es) of
    err :| errors ->
      P.vcat (toDocHelp err errors)


toDocHelp :: Error -> [Error] -> [P.Doc]
toDocHelp e1 errs =
  case errs of
    [] ->
      [errorToDoc e1]

    e2 : otherErrors ->
      errorToDoc e1 : separator (_name e1) (_name e2) : toDocHelp e2 otherErrors


errorToDoc :: Error -> P.Doc
errorToDoc (Error _name path _time source errors) =
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
