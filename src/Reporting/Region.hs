{-# LANGUAGE OverloadedStrings #-}
module Reporting.Region where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import Data.Binary



-- REGION


data Region =
  Region
    { _start :: !Position
    , _end :: !Position
    }
    deriving (Eq, Ord)


data Position =
  Position
    { _line :: !Int
    , _column :: !Int
    }
    deriving (Eq, Ord)


merge :: Region -> Region -> Region
merge (Region start _) (Region _ end) =
    Region start end



-- TO STRING


toString :: Region -> String
toString (Region (Position startLine startColumn) (Position endLine endColumn)) =
  case startLine == endLine of
    False ->
        "between lines " ++ show startLine
        ++ " and " ++ show endLine

    True ->
        "on line " ++ show endLine ++ ", column "
        ++ show startColumn ++ " to " ++ show endColumn



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



-- BINARY


instance Binary Region where
  get =
    Region <$> get <*> get

  put (Region start end) =
    do  put start
        put end


instance Binary Position where
  get =
    Position <$> get <*> get

  put (Position line column) =
    do  put line
        put column
