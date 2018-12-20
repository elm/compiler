{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Constraint
  ( Constraint
  , exactly
  , anything
  , toChars
  , toString
  , Error(..)
  , fromString
  , satisfies
  , check
  , intersect
  , goodElm
  , defaultElm
  , untilNextMajor
  , untilNextMinor
  , expand
  , decoder
  , encode
  )
  where


import Control.Monad (liftM4)
import Data.Binary (Binary, get, put, getWord8, putWord8)
import qualified Data.Utf8 as Utf8

import qualified Elm.Version as V
import qualified Json.Decode as D
import qualified Json.Encode as E



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



-- TO STRING


toChars :: Constraint -> String
toChars constraint =
  Utf8.toChars (toString constraint)


toString :: Constraint -> Utf8.String
toString constraint =
  case constraint of
    Range lower lowerOp upperOp upper ->
      Utf8.join 0x20 {- -}
        [ V.toString lower
        , toStringHelp lowerOp
        , "v"
        , toStringHelp upperOp
        , V.toString upper
        ]


toStringHelp :: Op -> Utf8.String
toStringHelp op =
  case op of
    Less        -> "<"
    LessOrEqual -> "<="



-- FROM STRING


data Error
  = BadLower Utf8.String
  | BadUpper Utf8.String
  | BadFormat
  | InvalidOp
  | InvalidRange V.Version V.Version


fromString :: Utf8.String -> Either Error Constraint
fromString string =
  case Utf8.split 0x20 {- -} string of
    [lower, lowerOp, "v", upperOp, upper] ->
      do  lo <- versionFromString lower BadLower
          lop <- opFromString lowerOp
          hop <- opFromString upperOp
          hi <- versionFromString upper BadUpper
          if lo < hi
            then Right (Range lo lop hop hi)
            else Left (InvalidRange lo hi)

    _ ->
      Left BadFormat


versionFromString :: Utf8.String -> (Utf8.String -> Error) -> Either Error V.Version
versionFromString string toError =
  case V.fromString string of
    Just vsn ->
      Right vsn

    Nothing ->
      Left (toError string)


opFromString :: Utf8.String -> Either Error Op
opFromString text =
  case text of
    "<=" -> Right LessOrEqual
    "<"  -> Right Less
    _    -> Left InvalidOp



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
  E.string (toString constraint)


decoder :: D.Decoder Error Constraint
decoder =
  do  str <- D.string
      case fromString str of
        Right constraint ->
          return constraint

        Left err ->
          D.failure err



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
          _ -> error "binary encoding of Op was corrupted"