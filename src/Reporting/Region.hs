{-# LANGUAGE OverloadedStrings #-}
module Reporting.Region where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Text.Parsec.Pos as Parsec


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
