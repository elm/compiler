{-# LANGUAGE OverloadedStrings #-}
module Reporting.Region
  ( Region(..)
  , Position(..)
  , zero
  , one
  , merge
  , encode
  )
  where


import qualified Json.Encode as Json
import Data.Binary (Binary, get, put)



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


{-# NOINLINE zero #-}
zero :: Region
zero =
  Region (Position 0 0) (Position 0 0)


{-# NOINLINE one #-}
one :: Region
one =
  Region (Position 1 1) (Position 1 1)



-- JSON


encode :: Region -> Json.Value
encode (Region start end) =
  Json.object
    [ ("start", encodePosition start)
    , ("end", encodePosition end)
    ]


encodePosition :: Position -> Json.Value
encodePosition (Position line column) =
  Json.object
    [ ("line", Json.int line)
    , ("column", Json.int column)
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
