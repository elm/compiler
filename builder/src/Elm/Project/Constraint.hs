{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project.Constraint
  ( Constraint
  , exactly
  , anything
  , fromText
  , toString
  , toText
  , satisfies
  , check
  , intersect
  , goodElm
  , defaultElm
  , untilNextMajor
  , untilNextMinor
  , expand
  )
  where


import Control.Monad (liftM4)
import Data.Binary
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Pkg
import Elm.Package (Version(..))



-- CONSTRAINTS


data Constraint
    = Range Version Op Op Version
    deriving (Eq)


data Op
  = Less
  | LessOrEqual
  deriving (Eq)



-- COMMON CONSTRAINTS


exactly :: Version -> Constraint
exactly version =
  Range version LessOrEqual LessOrEqual version


anything :: Constraint
anything =
  Range (Version 1 0 0) LessOrEqual LessOrEqual (Version maxBound 0 0)



-- TEXT CONVERSION


toString :: Constraint -> String
toString constraint =
  Text.unpack (toText constraint)


toText :: Constraint -> Text
toText constraint =
  case constraint of
    Range lower lowerOp upperOp upper ->
      Text.intercalate " "
        [ Pkg.versionToText lower
        , opToText lowerOp
        , "v"
        , opToText upperOp
        , Pkg.versionToText upper
        ]


opToText :: Op -> Text
opToText op =
  case op of
    Less ->
      "<"

    LessOrEqual ->
      "<="


fromText :: Text -> Either String Constraint
fromText text =
  case Text.splitOn " " text of
    [lower, lowerOp, "v", upperOp, upper] ->
      do  lo <- versionFromText "lower" lower
          lop <- opFromText lowerOp
          hop <- opFromText upperOp
          hi <- versionFromText "upper" upper
          if lo < hi
            then Right (Range lo lop hop hi)
            else
              Left $
                "The lower bound (" ++ Text.unpack lower
                ++ ") must be less than the upper bound ("
                ++ Text.unpack upper ++ ")"

    _ ->
      Left "The standard format is \"1.0.0 <= v < 2.0.0\""


versionFromText :: String -> Text -> Either String Version
versionFromText lowerOrUpper text =
  case Pkg.versionFromText text of
    Just vsn ->
      Right vsn

    Nothing ->
      Left $ "The " ++ lowerOrUpper ++ " bound (" ++ Text.unpack text ++ ") is not a valid version number."


opFromText :: Text -> Either String Op
opFromText text =
  case text of
    "<=" ->
      Right LessOrEqual

    "<" ->
      Right Less

    _ ->
      Left $ "The operator (" ++ Text.unpack text ++ ") is not valid. Must be (<=) or (<)."



-- IS SATISFIED


satisfies :: Constraint -> Version -> Bool
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


check :: Constraint -> Version -> Ordering
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
  satisfies constraint Compiler.version


defaultElm :: Constraint
defaultElm =
  if Pkg._major Compiler.version > 0
    then untilNextMajor Compiler.version
    else untilNextMinor Compiler.version



-- CREATE CONSTRAINTS


untilNextMajor :: Version -> Constraint
untilNextMajor version =
  Range version LessOrEqual Less (Pkg.bumpMajor version)


untilNextMinor :: Version -> Constraint
untilNextMinor version =
  Range version LessOrEqual Less (Pkg.bumpMinor version)


expand :: Constraint -> Version -> Constraint
expand constraint@(Range lower lowerOp upperOp upper) version
  | version < lower =
      Range version LessOrEqual upperOp upper

  | version > upper =
      Range lower lowerOp Less (Pkg.bumpMajor version)

  | otherwise =
      constraint



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