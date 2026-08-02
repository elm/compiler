{-# LANGUAGE BangPatterns, BinaryLiterals, ExtendedLiterals, MagicHash,
TemplateHaskell, UnboxedTuples
#-}
module Elm.Version
  ( Version
  , toVersion
  , fromVersion
  --
  , one
  , max
  --
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
  --
  , eVersion, dVersion
  )
  where


import Prelude hiding (max)
import qualified Data.Version as Version
import GHC.Exts (isTrue#)
import GHC.Prim
import GHC.Word (Word8(..), Word16(..))
import qualified Paths_elm

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Crash

import qualified Json.Decode as JD
import qualified Json.Encode as JE
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


toVersion :: Word16 -> Word16 -> Word16 -> Version
toVersion =
  Version


fromVersion :: Version -> (Word16 -> Word16 -> Word16 -> r) -> r
fromVersion (Version x y z) cont =
  cont x y z



-- COMMON VERSIONS


one :: Version
one =
  toVersion 1 0 0


max :: Version
max =
  toVersion maxBound 0 0



-- COMPILER VERSION


compiler :: Version
compiler =
  case map fromIntegral (Version.versionBranch Paths_elm.version) of
    [x,y,z] -> toVersion x y z
    [x,y]   -> toVersion x y 0
    [x]     -> toVersion x 0 0
    _       -> $(Crash.crash 'compiler) "could not detect compiler version from cabal file"



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
toChars vsn =
  fromVersion vsn $ \major minor patch ->
    show major ++ '.' : show minor ++ '.' : show patch



-- JSON


decoder :: JD.Decoder A.Position Version
decoder =
  JD.customString parser A.Position


encode :: Version -> JE.Value
encode version =
  JE.chars (toChars version)



-- BINARY


dVersion :: D.Decoder Version
dVersion =
  do  word <- D.u8
      if word == 255
        then
          do  x <- D.u16
              y <- D.u16
              z <- D.u16
              pure $ toVersion (fromIntegral x) (fromIntegral y) (fromIntegral z)
        else
          do  minor <- D.u8
              patch <- D.u8
              return (toVersion (fromIntegral word) (fromIntegral minor) (fromIntegral patch))


eVersion :: Version -> E.Builder
eVersion vsn =
  fromVersion vsn $ \major minor patch ->
    if major < 255 && minor < 256 && patch < 256
    then E.u8 (fromIntegral major) <> E.u8 (fromIntegral minor) <> E.u8 (fromIntegral patch)
    else E.u8 255 <> E.u16 major <> E.u16 minor <> E.u16 patch



-- PARSER


parser :: P.Parser A.Position Version
parser =
  do  major <- numberParser
      P.word1 0x2E#Word8 {-.-} A.Position
      minor <- numberParser
      P.word1 0x2E#Word8 {-.-} A.Position
      patch <- numberParser
      return (toVersion major minor patch)


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
