{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse.Kernel
  ( parser
  )
  where


import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

import qualified AST.Exposing as Exposing
import qualified AST.Kernel as Kernel
import qualified AST.Module as Module
import qualified AST.Module.Name as Module
import Parse.Helpers (Parser)
import qualified Parse.Helpers as Parse
import qualified Parse.Module as Parse
import qualified Reporting.Annotation as A



-- PARSER


parser :: Parser Kernel.Info
parser =
  do  imports <- Parse.kernelHeader
      let table = Map.unions (map importToTable imports)
      chunks <- parserHelp table []
      return (Kernel.Info (Map.elems table) chunks)


parserHelp :: Table -> [Kernel.Chunk] -> Parser [Kernel.Chunk]
parserHelp table chunks =
  do  chunk <- Parse.kernelChunk
      case chunk of
        Left js ->
          return (Kernel.JS js : chunks)

        Right (js, var) ->
          case Map.lookup var table of
            Nothing ->
              error $ show $ "could not find " <> var <> " when parsing kernel code"

            Just (home, name) ->
              parserHelp table (Kernel.Var home name : Kernel.JS js : chunks)



-- SYMBOL TABLE


type Table =
  Map.Map Text (Module.Raw, Text)


importToTable :: Module.UserImport -> Table
importToTable (A.A _ ( fullName, Module.ImportMethod maybeAlias exposed )) =
  let
    shortName =
      case maybeAlias of
        Just alias ->
          alias

        Nothing ->
          if Module.isKernel fullName then
            Module.getKernel fullName
          else if Text.isInfixOf "." fullName then
            error ("modules with dots in kernel code need an alias: " ++ show fullName)
          else
            fullName

    toEntry value =
      ( "__" <> shortName <> "_" <> value, ( fullName, value ) )
  in
    Map.fromList $ map toEntry $
      concatMap entryToValues $ getExplicits exposed


getExplicits :: Exposing.Exposing a -> [a]
getExplicits exposed =
  case exposed of
    Exposing.Open ->
      error "cannot have open imports in kernel code"

    Exposing.Explicit entries ->
      map A.drop entries


entryToValues :: Exposing.Entry -> [Text]
entryToValues entry =
  case entry of
    Exposing.Lower name ->
      [name]

    Exposing.Upper name ctors ->
      maybe [name] getExplicits ctors
