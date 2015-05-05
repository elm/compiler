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
          |> drop (line start - 1)
          |> take (line end - line start + 1)
  in
  case relevantLines of
    [] ->
        error "something has gone badly wrong with reporting source locations"

    [theLine] ->
        let
          n = line start
          w = length (show (n + 1))
          underline =
              replicate (column start - 1) ' '
              ++ replicate (max 1 (column end - column start)) '^'
        in
            unlines
              [ addLineNumber w (n-1) ""
              , addLineNumber w n theLine
              , addLineNumber w (n+1) underline
              ]

    firstLine : rest ->
        let
          filteredFirstLine =
              replicate (column start) ' ' ++ drop (column start) firstLine

          filteredLastLine =
              take (column end) (last rest)

          focusedRelevantLines =
              filteredFirstLine : init rest ++ [filteredLastLine]

          lineNumbersWidth =
              length (show (line end + 1))

          numberedLines =
              zipWith
                (addLineNumber lineNumbersWidth)
                [line start - 1 .. line end + 1]
                ("" : focusedRelevantLines ++ [""])
        in
            unlines numberedLines ++ "\n"


addLineNumber :: Int -> Int -> String -> String
addLineNumber width n line =
  let
    number = if n < 0 then " " else show n
  in
    replicate (width - length number) ' ' ++ number ++ "| " ++ line


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
