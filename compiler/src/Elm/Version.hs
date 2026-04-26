{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, UnboxedTuples #-}
module Elm.Version
  ( Version(..)
  , one
  , max
  , compiler
  , bumpPatch
  , bumpMinor
  , bumpMajor
  , toChars
  --
  , decoder
  , encode
  --
  , parser
  )
  where


import Prelude hiding (max)
import Control.Monad (liftM3)
import Data.Binary (Binary, get, put, getWord8, putWord8)
import qualified Data.Version as Version
import GHC.Exts (isTrue#)
import GHC.Prim
import GHC.Word (Word8(..), Word16)
import qualified Paths_elm

import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A



-- VERSION


data Version =
  Version
    { _major :: {-# UNPACK #-} !Word16
    , _minor :: {-# UNPACK #-} !Word16
    , _patch :: {-# UNPACK #-} !Word16
    }
    deriving (Eq, Ord)


one :: Version
one =
  Version 1 0 0


max :: Version
max =
  Version maxBound 0 0


compiler :: Version
compiler =
  case map fromIntegral (Version.versionBranch Paths_elm.version) of
    major : minor : patch : _ ->
      Version major minor patch

    [major, minor] ->
      Version major minor 0

    [major] ->
      Version major 0 0

    [] ->
      error "could not detect version of elm-compiler you are using"



-- BUMP


bumpPatch :: Version -> Version
bumpPatch (Version major minor patch) =
  Version major minor (patch + 1)


bumpMinor :: Version -> Version
bumpMinor (Version major minor _patch) =
  Version major (minor + 1) 0


bumpMajor :: Version -> Version
bumpMajor (Version major _minor _patch) =
  Version (major + 1) 0 0



-- TO CHARS


toChars :: Version -> [Char]
toChars (Version major minor patch) =
  show major ++ '.' : show minor ++ '.' : show patch



-- JSON


decoder :: D.Decoder A.Position Version
decoder =
  D.customString parser A.Position


encode :: Version -> E.Value
encode version =
  E.chars (toChars version)



-- BINARY


instance Binary Version where
  get =
    do  word <- getWord8
        if word == 255
          then liftM3 Version get get get
          else
            do  minor <- getWord8
                patch <- getWord8
                return (Version (fromIntegral word) (fromIntegral minor) (fromIntegral patch))

  put (Version major minor patch) =
    if major < 255 && minor < 256 && patch < 256 then
      do  putWord8 (fromIntegral major)
          putWord8 (fromIntegral minor)
          putWord8 (fromIntegral patch)
    else
      do  putWord8 255
          put major
          put minor
          put patch



-- PARSER


parser :: P.Parser A.Position Version
parser =
  do  major <- numberParser
      P.word1 0x2E#Word8 {-.-} A.Position
      minor <- numberParser
      P.word1 0x2E#Word8 {-.-} A.Position
      patch <- numberParser
      return (Version major minor patch)


numberParser :: P.Parser A.Position Word16
numberParser =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    if P.notLtAddr pos end then
      eerr cur A.Position
    else
      let !word = indexWord8OffAddr# pos 0# in
      if isTrue# (eqWord8# word 0x30#Word8 {-0-}) then

        let
          !newState = P.State (plusAddr# pos 1#) end indent (P.slide cur 1#Word64)
        in
        cok 0 newState

      else if isDigit word then

        let
          !(# total, newPos #) = chompWord16 (plusAddr# pos 1#) end (fromIntegral (W8# word - 0x30))
          !newState = P.State newPos end indent (P.slide cur (wordToWord64# (int2Word# (minusAddr# newPos pos))))
        in
        cok total newState

      else
        eerr cur A.Position


chompWord16 :: Addr# -> Addr# -> Word16 -> (# Word16, Addr# #)
chompWord16 pos end total =
  if P.notLtAddr pos end then
    (# total, pos #)
  else
    let !word = indexWord8OffAddr# pos 0# in
    if isDigit word then
      chompWord16 (plusAddr# pos 1#) end (10 * total + fromIntegral (W8# word - 0x30))
    else
      (# total, pos #)


isDigit :: Word8# -> Bool
isDigit word =
  isTrue# (0x30#Word8 {-0-} `leWord8#` word) && isTrue# (word `leWord8#` 0x39#Word8 {-9-})
