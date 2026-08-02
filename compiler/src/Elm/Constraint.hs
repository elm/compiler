{-# LANGUAGE ExtendedLiterals, MagicHash, OverloadedStrings #-}
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
  --
  , dConstraint, eConstraint
  )
  where


import Control.Monad (liftM4)

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E

import qualified Elm.Version as V
import qualified Json.Decode as JD
import qualified Json.Encode as JE
import qualified Parse.Primitives as P
import Parse.Primitives (Cursor)
import qualified Reporting.Annotation as A



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
  V.fromVersion V.compiler $ \major _ _ ->
    if major > 0
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


encode :: Constraint -> JE.Value
encode constraint =
  JE.chars (toChars constraint)


decoder :: JD.Decoder Error Constraint
decoder =
  JD.customString parser BadFormat



-- BINARY


dConstraint :: D.Decoder Constraint
dConstraint =
  liftM4 Range V.dVersion dOp dOp V.dVersion


eConstraint :: Constraint -> E.Builder
eConstraint (Range lo op op' hi) =
  V.eVersion lo <> eOp op <> eOp op' <> V.eVersion hi


dOp :: D.Decoder Op
dOp =
  do  n <- D.u8
      case n of
        0 -> pure Less
        1 -> pure LessOrEqual
        _ -> D.expecting "Op"


eOp :: Op -> E.Builder
eOp op =
  case op of
    Less        -> E.u8# 0#Word8
    LessOrEqual -> E.u8# 1#Word8



-- PARSER


data Error
  = BadFormat Cursor
  | InvalidRange V.Version V.Version


parser :: P.Parser Error Constraint
parser =
  do  lower <- parseVersion
      P.word1 0x20#Word8 {- -} BadFormat
      loOp <- parseOp
      P.word1 0x20#Word8 {- -} BadFormat
      P.word1 0x76#Word8 {-v-} BadFormat
      P.word1 0x20#Word8 {- -} BadFormat
      hiOp <- parseOp
      P.word1 0x20#Word8 {- -} BadFormat
      higher <- parseVersion
      P.Parser $ \_ state@(P.State _ _ _ cur) _ eok _ eerr ->
        if lower < higher
        then eok (Range lower loOp hiOp higher) state
        else eerr cur (\_ -> InvalidRange lower higher)


parseVersion :: P.Parser Error V.Version
parseVersion =
  P.specialize (\(A.Position c) _ -> BadFormat c) V.parser


parseOp :: P.Parser Error Op
parseOp =
  do  P.word1 0x3C#Word8 {-<-} BadFormat
      P.oneOfWithFallback
        [ do  P.word1 0x3D#Word8 {-=-} BadFormat
              return LessOrEqual
        ]
        Less
