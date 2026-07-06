{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash #-}
module AST.Prim.Variable
  ( chompUpper
  , chompLower
  , chompInner
  )
  where


import qualified Data.Char as Char
import GHC.Base (Char(C#))
import GHC.Exts (isTrue#)
import GHC.Prim

import qualified Bytes
import qualified Literals



-- CHOMPERS


{-# INLINE chompUpper #-}
{-# INLINE chompLower #-}
{-# INLINE chompInner #-}

chompUpper :: Addr# -> Addr# -> Word8# -> Addr#
chompLower :: Addr# -> Addr# -> Word8# -> Addr#
chompInner :: Addr# -> Addr# -> Word8# -> Addr#

chompUpper = chomp isUpper Char.isUpper
chompLower = chomp isLower Char.isLower
chompInner = chomp isInner Char.isAlpha



-- ASCII HELPERS


{-# INLINE isUpper #-}
{-# INLINE isLower #-}
{-# INLINE isDigit #-}
{-# INLINE isInner #-}

isUpper :: Word8# -> Bool
isLower :: Word8# -> Bool
isDigit :: Word8# -> Bool
isInner :: Word8# -> Bool

isUpper w = isTrue# (leWord8# 0x41#Word8 w) && isTrue# (leWord8# w 0x5A#Word8)
isLower w = isTrue# (leWord8# 0x61#Word8 w) && isTrue# (leWord8# w 0x7A#Word8)
isDigit w = isTrue# (leWord8# 0x30#Word8 w) && isTrue# (leWord8# w 0x39#Word8)
isInner w = isLower w || isUpper w || isDigit w || isTrue# (eqWord8# w 0x5F#Word8 {-_-})



-- CHOMP HELPER


{-# INLINE chomp #-}
chomp :: (Word8# -> Bool) -> (Char -> Bool) -> Addr# -> Addr# -> Word8# -> Addr#
chomp isGood# isGood pos end w
  | isGood# w                       = plusAddr# pos 1#
  | isTrue# (ltWord8# w 0xC0#Word8) = pos
  | isTrue# (ltWord8# w 0xE0#Word8) = chomp2 isGood pos end w
  | isTrue# (ltWord8# w 0xF0#Word8) = chomp3 isGood pos end w
  | isTrue# (ltWord8# w 0xF8#Word8) = chomp4 isGood pos end w
  | otherwise                       = pos


chomp2 :: (Char -> Bool) -> Addr# -> Addr# -> Word8# -> Addr#
chomp2 isGood pos end word =
  if isTrue# (leAddr# pos2 end) &&
  (
    let
      !w1 = word8ToWord# word
      !w2 = getWord8# pos 1#

      !c1 = uncheckedShiftL# (and# 0x3F## w1) 6#
      !c2 = and# 0x3F## w2
    in
    isTrue# (eqWord8# 0#Word8 (step table w2 (step table w1 0#Word8)))
    && isGood (C# (chr# (word2Int# (or# c1 c2))))
  )
    then pos2
    else pos
  where
    !pos2 = plusAddr# pos 2#
    !(Literals.FSM table) = Bytes.utf8_fsm


chomp3 :: (Char -> Bool) -> Addr# -> Addr# -> Word8# -> Addr#
chomp3 isGood pos end word =
  if isTrue# (leAddr# pos3 end) &&
  (
    let
      !w1 = word8ToWord# word
      !w2 = getWord8# pos 1#
      !w3 = getWord8# pos 2#

      !c1 = uncheckedShiftL# (and# 0x0F## w1) 12#
      !c2 = uncheckedShiftL# (and# 0x3F## w2) 6#
      !c3 = and# 0x3F## w3
    in
    isTrue# (eqWord8# 0#Word8 (step table w3 (step table w2 (step table w1 0#Word8))))
    &&
    isGood (C# (chr# (word2Int# (or# c1 (or# c2 c3)))))
  )
    then pos3
    else pos
  where
    !pos3 = plusAddr# pos 3#
    !(Literals.FSM table) = Bytes.utf8_fsm


chomp4 :: (Char -> Bool) -> Addr# -> Addr# -> Word8# -> Addr#
chomp4 isGood pos end word =
  if isTrue# (leAddr# pos4 end) &&
  (
    let
      !w1 = word8ToWord# word
      !w2 = getWord8# pos 1#
      !w3 = getWord8# pos 2#
      !w4 = getWord8# pos 3#

      !c1 = uncheckedShiftL# (and# 0x07## w1) 18#
      !c2 = uncheckedShiftL# (and# 0x3F## w2) 12#
      !c3 = uncheckedShiftL# (and# 0x3F## w3) 6#
      !c4 = and# 0x3F## w4
    in
    isTrue# (eqWord8# 0#Word8 (step table w4 (step table w3 (step table w2 (step table w1 0#Word8)))))
    &&
    isGood (C# (chr# (word2Int# (or# (or# c1 c2) (or# c3 c4)))))
  )
    then pos4
    else pos
  where
    !pos4 = plusAddr# pos 4#
    !(Literals.FSM table) = Bytes.utf8_fsm


{-# INLINE getWord8# #-}
getWord8# :: Addr# -> Int# -> Word#
getWord8# ba i =
  word8ToWord# (indexWord8OffAddr# ba i)


{-# INLINE step #-}
step :: Addr# -> Word# -> Word8# -> Word8#
step table word state =
  let
    !tran = indexInt8OffAddr# table (word2Int# word)
  in
  indexWord8OffAddr# table (256# +# word2Int# (word8ToWord# state) +# int8ToInt# tran)

