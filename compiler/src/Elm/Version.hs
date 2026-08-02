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
import GHC.Word (Word8(..), Word32(..), Word64(..))
import qualified Paths_elm

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Crash

import qualified Json.Decode as JD
import qualified Json.Encode as JE
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A



-- VERSION
--
-- 21-bits for major and minor (2^21 = 2M)
-- 22-bits for patch (2^22 = 4M)


newtype Version = Version Word64
  deriving (Eq, Ord)


toVersion :: Word32 -> Word32 -> Word32 -> Version
toVersion (W32# major) (W32# minor) (W32# patch) =
  Version $ W64# $
    uncheckedShiftL64# (wordToWord64# (word32ToWord# major)) 43#
    `or64#`
    uncheckedShiftL64# (and64# 0b111111111111111111111#Word64 (wordToWord64# (word32ToWord# minor))) 22#
    `or64#`
    and64# 0b1111111111111111111111#Word64 (wordToWord64# (word32ToWord# patch))


fromVersion :: Version -> (Word32 -> Word32 -> Word32 -> r) -> r
fromVersion (Version (W64# vsn)) cont =
  cont
    (W32# (wordToWord32# (word64ToWord# (uncheckedShiftRL64# vsn 43#                                         ))))
    (W32# (wordToWord32# (word64ToWord# (uncheckedShiftRL64# vsn 22# `and64#` 0b111111111111111111111#Word64 ))))
    (W32# (wordToWord32# (word64ToWord# (                    vsn     `and64#` 0b1111111111111111111111#Word64))))



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
bumpPatch (Version vsn) =
  Version (vsn + 1)


bumpMinor :: Version -> Version
bumpMinor (Version (W64# vsn)) =
  Version $ W64# $
    (vsn `and64#` 0b1111111111111111111111111111111111111111110000000000000000000000#Word64) `plusWord64#` 0b10000000000000000000000#Word64


bumpMajor :: Version -> Version
bumpMajor (Version (W64# vsn)) =
  Version $ W64# $
    (vsn `and64#` 0b1111111111111111111110000000000000000000000000000000000000000000#Word64) `plusWord64#` 0b10000000000000000000000000000000000000000000#Word64



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
    else E.u8 255 <> E.u16 (fromIntegral major) <> E.u16 (fromIntegral minor) <> E.u16 (fromIntegral patch)



-- PARSER


parser :: P.Parser A.Position Version
parser =
  do  major <- numberParser
      P.word1 0x2E#Word8 {-.-} A.Position
      minor <- numberParser
      P.word1 0x2E#Word8 {-.-} A.Position
      patch <- numberParser
      return (toVersion major minor patch)


numberParser :: P.Parser A.Position Word32
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
          !(# total, newPos #) = chompWord32 (plusAddr# pos 1#) end (fromIntegral (W8# word - 0x30))
          !newState = P.State newPos end indent (P.slide cur (wordToWord64# (int2Word# (minusAddr# newPos pos))))
        in
        cok total newState

      else
        eerr cur A.Position


chompWord32 :: Addr# -> Addr# -> Word32 -> (# Word32, Addr# #)
chompWord32 pos end total =
  if P.notLtAddr pos end then
    (# total, pos #)
  else
    let !word = indexWord8OffAddr# pos 0# in
    if isDigit word then
      chompWord32 (plusAddr# pos 1#) end (10 * total + fromIntegral (W8# word - 0x30))
    else
      (# total, pos #)


isDigit :: Word8# -> Bool
isDigit word =
  isTrue# (0x30#Word8 {-0-} `leWord8#` word) && isTrue# (word `leWord8#` 0x39#Word8 {-9-})

