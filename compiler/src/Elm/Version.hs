{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, UnboxedTuples #-}
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
import Data.Word (Word8, Word16)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import qualified Paths_elm

import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Parse.Primitives as P
import Parse.Primitives (Row, Col)



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


decoder :: D.Decoder (Row, Col) Version
decoder =
  D.customString parser (,)


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


parser :: P.Parser (Row, Col) Version
parser =
  do  major <- numberParser
      P.word1 0x2E {-.-} (,)
      minor <- numberParser
      P.word1 0x2E {-.-} (,)
      patch <- numberParser
      return (Version major minor patch)


numberParser :: P.Parser (Row, Col) Word16
numberParser =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    if pos >= end then
      eerr row col (,)
    else
      let !word = P.unsafeIndex pos in
      if word == 0x30 {-0-} then

        let
          !newState = P.State src (plusPtr pos 1) end indent row (col + 1)
        in
        cok 0 newState

      else if isDigit word then

        let
          (# total, newPos #) = chompWord16 (plusPtr pos 1) end (fromIntegral (word - 0x30))
          !newState = P.State src newPos end indent row (col + fromIntegral (minusPtr newPos pos))
        in
        cok total newState

      else
        eerr row col (,)


chompWord16 :: Ptr Word8 -> Ptr Word8 -> Word16 -> (# Word16, Ptr Word8 #)
chompWord16 pos end total =
  if pos >= end then
    (# total, pos #)
  else
    let !word = P.unsafeIndex pos in
    if isDigit word then
      chompWord16 (plusPtr pos 1) end (10 * total + fromIntegral (word - 0x30))
    else
      (# total, pos #)


isDigit :: Word8 -> Bool
isDigit word =
  0x30 {-0-} <= word && word <= 0x39 {-9-}
