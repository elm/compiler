{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.IntMap as IntMap
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified GHC.RTS.TimeAllocProfile as Prof
import qualified GHC.RTS.TimeAllocProfile.Types as Prof
import qualified Text.Tabular as Table
import qualified Text.Tabular.AsciiArt as Table



main :: IO ()
main =
  profToTable "elm-make.prof"


profToTable :: FilePath -> IO ()
profToTable filePath =
  do  text <- Text.readFile filePath
      case Atto.parseOnly Prof.timeAllocProfile text of
        Left err ->
          putStrLn $ "Error parsing " ++ filePath ++ ": " ++ err

        Right profile ->
          printTable (extractCostCentres profile)



-- FILTER NAMED COST CENTRES


extractCostCentres :: Prof.TimeAllocProfile -> [Prof.CostCentre]
extractCostCentres profile =
  let
    nodes =
      IntMap.elems (Prof.costCentreNodes (Prof.profileCostCentreTree profile))
  in
    filter (Text.isPrefixOf "elm_" . Prof.costCentreName) nodes



-- CREATE A TABLE


printTable :: [Prof.CostCentre] -> IO ()
printTable costCentres =
  let
    rowHeaders =
      Table.Group Table.NoLine $
        map (Table.Header . Text.unpack . Prof.costCentreName) costCentres

    table =
      Table.Table rowHeaders colHeaders (map toRow costCentres)
  in
    putStrLn (Table.render id id id table)


colHeaders :: Table.Header String
colHeaders =
  Table.Group Table.NoLine
    [ Table.Header "  total time"
    , Table.Header "  local time"
    , Table.Header "  total alloc"
    , Table.Header "  local alloc"
    ]


toRow :: Prof.CostCentre -> [String]
toRow costCentre =
  [ show $ Prof.costCentreInhTime costCentre
  , show $ Prof.costCentreIndTime costCentre
  , show $ Prof.costCentreInhAlloc costCentre
  , show $ Prof.costCentreIndAlloc costCentre
  ]