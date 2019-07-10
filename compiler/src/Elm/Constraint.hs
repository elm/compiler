{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Constraint
  ( Constraint
  , exactly
  , anything
  , toChars
  , satisfies
  , check
  , intersect
  , goodElm
  , defaultElm
  , untilNextMajor
  , untilNextMinor
  , expand
  --
  , Error(..)
  , decoder
  , encode
  )
  where


import Control.Monad (liftM4)
import Data.Binary (Binary, get, put, getWord8, putWord8)

import qualified Elm.Version as V
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Parse.Primitives as P
import Parse.Primitives (Row, Col)



-- CONSTRAINTS


data Constraint
    = Range V.Version Op Op V.Version
    deriving (Eq)


data Op
  = Less
  | LessOrEqual
  deriving (Eq)



-- COMMON CONSTRAINTS


exactly :: V.Version -> Constraint
exactly version =
  Range version LessOrEqual LessOrEqual version


anything :: Constraint
anything =
  Range V.one LessOrEqual LessOrEqual V.max



-- TO CHARS


toChars :: Constraint -> [Char]
toChars constraint =
  case constraint of
    Range lower lowerOp upperOp upper ->
      V.toChars lower ++ opToChars lowerOp ++ "v" ++ opToChars upperOp ++ V.toChars upper


opToChars :: Op -> [Char]
opToChars op =
  case op of
    Less        -> " < "
    LessOrEqual -> " <= "



-- IS SATISFIED


satisfies :: Constraint -> V.Version -> Bool
satisfies constraint version =
  case constraint of
    Range lower lowerOp upperOp upper ->
        isLess lowerOp lower version
          &&
        isLess upperOp version upper


isLess :: (Ord a) => Op -> (a -> a -> Bool)
isLess op =
  case op of
    Less ->
      (<)

    LessOrEqual ->
      (<=)


check :: Constraint -> V.Version -> Ordering
check constraint version =
  case constraint of
    Range lower lowerOp upperOp upper ->
      if not (isLess lowerOp lower version) then
        LT

      else if not (isLess upperOp version upper) then
        GT

      else
        EQ



-- INTERSECT


intersect :: Constraint -> Constraint -> Maybe Constraint
intersect (Range lo lop hop hi) (Range lo_ lop_ hop_ hi_) =
  let
    (newLo, newLop) =
      case compare lo lo_ of
        LT -> (lo_, lop_)
        EQ -> (lo, if elem Less [lop,lop_] then Less else LessOrEqual)
        GT -> (lo, lop)

    (newHi, newHop) =
      case compare hi hi_ of
        LT -> (hi, hop)
        EQ -> (hi, if elem Less [hop, hop_] then Less else LessOrEqual)
        GT -> (hi_, hop_)
  in
    if newLo <= newHi then
      Just (Range newLo newLop newHop newHi)
    else
      Nothing



-- ELM CONSTRAINT


goodElm :: Constraint -> Bool
goodElm constraint =
  satisfies constraint V.compiler


defaultElm :: Constraint
defaultElm =
  if V._major V.compiler > 0
    then untilNextMajor V.compiler
    else untilNextMinor V.compiler



-- CREATE CONSTRAINTS


untilNextMajor :: V.Version -> Constraint
untilNextMajor version =
  Range version LessOrEqual Less (V.bumpMajor version)


untilNextMinor :: V.Version -> Constraint
untilNextMinor version =
  Range version LessOrEqual Less (V.bumpMinor version)


expand :: Constraint -> V.Version -> Constraint
expand constraint@(Range lower lowerOp upperOp upper) version
  | version < lower =
      Range version LessOrEqual upperOp upper

  | version > upper =
      Range lower lowerOp Less (V.bumpMajor version)

  | otherwise =
      constraint



-- JSON


encode :: Constraint -> E.Value
encode constraint =
  E.chars (toChars constraint)


decoder :: D.Decoder Error Constraint
decoder =
  D.customString parser BadFormat



-- BINARY


instance Binary Constraint where
  get = liftM4 Range get get get get
  put (Range a b c d) = put a >> put b >> put c >> put d


instance Binary Op where
  put op =
    case op of
      Less        -> putWord8 0
      LessOrEqual -> putWord8 1

  get =
    do  n <- getWord8
        case n of
          0 -> return Less
          1 -> return LessOrEqual
          _ -> fail "binary encoding of Op was corrupted"



-- PARSER


data Error
  = BadFormat Row Col
  | InvalidRange V.Version V.Version


parser :: P.Parser Error Constraint
parser =
  do  lower <- parseVersion
      P.word1 0x20 {- -} BadFormat
      loOp <- parseOp
      P.word1 0x20 {- -} BadFormat
      P.word1 0x76 {-v-} BadFormat
      P.word1 0x20 {- -} BadFormat
      hiOp <- parseOp
      P.word1 0x20 {- -} BadFormat
      higher <- parseVersion
      P.Parser $ \state@(P.State _ _ _ _ row col) _ eok _ eerr ->
        if lower < higher
        then eok (Range lower loOp hiOp higher) state
        else eerr row col (\_ _ -> InvalidRange lower higher)


parseVersion :: P.Parser Error V.Version
parseVersion =
  P.specialize (\(r,c) _ _ -> BadFormat r c) V.parser


parseOp :: P.Parser Error Op
parseOp =
  do  P.word1 0x3C {-<-} BadFormat
      P.oneOfWithFallback
        [ do  P.word1 0x3D {-=-} BadFormat
              return LessOrEqual
        ]
        Less
