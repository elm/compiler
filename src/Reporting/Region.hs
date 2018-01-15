{-# LANGUAGE OverloadedStrings #-}
module Reporting.Region
  ( Region(..)
  , Position(..)
  , merge
  )
  where

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
