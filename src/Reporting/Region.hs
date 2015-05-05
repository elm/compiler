{-# LANGUAGE OverloadedStrings #-}
module Reporting.Region where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Text.Parsec.Pos as Parsec

import Elm.Utils ((|>))


data Region = Region
    { start :: Position
    , end :: Position
    }
    deriving (Show)


data Position = Position
    { line :: Int
    , column :: Int
    }
    deriving (Show)


fromSourcePos :: Parsec.SourcePos -> Position
fromSourcePos sourcePos =
    Position
      (Parsec.sourceLine sourcePos)
      (Parsec.sourceColumn sourcePos)


merge :: Region -> Region -> Region
merge (Region start _) (Region _ end) =
    Region start end


-- TO STRING

toString :: Region -> String
toString (Region start end) =
  case line start == line end of
    False ->
        "between lines " ++ show (line start)
        ++ " and " ++ show (line end)

    True ->
        "on line " ++ show (line end) ++ ", column "
        ++ show (column start) ++ " to " ++ show (column end)


-- EXTRACT REGION from SOURCE with LINE NUMBERS

select :: Region -> String -> String
select (Region start end) source =
  let
    relevantLines =
        lines source
          |> drop (line start)
          |> take (line end - line start)
  in
  case relevantLines of
    [] ->
        error "something has gone badly wrong with reporting source locations"

    [theLine] ->
        let
          lineNumber = show (line start) ++ "|"
        in
            lineNumber
            ++ theLine
            ++ "\n"
            ++ replicate (column start + length lineNumber) ' '
            ++ replicate (column end - column start) '^'
            ++ "\n"

    firstLine : rest ->
        let
          filteredFirstLine =
              replicate (column start) ' ' ++ drop (column start) firstLine

          filteredLastLine =
              take (column end) (last rest)

          focusedRelevantLines =
              filteredFirstLine : init rest ++ [filteredLastLine]

          lineNumbersWidth =
              length (show (line end))

          numberedLines =
              zipWith
                (addLineNumber lineNumbersWidth)
                [line start .. line end]
                focusedRelevantLines
        in
            unlines numberedLines


addLineNumber :: Int -> Int -> String -> String
addLineNumber width n line =
  let
    number = show n
  in
    replicate (width - length number) ' ' ++ number ++ "|" ++ line


-- JSON

instance Json.ToJSON Region where
  toJSON (Region start end) =
      Json.object
        [ "start" .= start
        , "end" .= end
        ]


instance Json.ToJSON Position where
  toJSON (Position line column) =
      Json.object
        [ "line" .= line
        , "column" .= column
        ]
