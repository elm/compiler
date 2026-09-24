{-# LANGUAGE BangPatterns, MagicHash, TemplateHaskell, UnboxedTuples #-}
module String
  ( String(..)
  , empty
  , isEmpty
  , size
  , size#
  , toBuilder
  , toRangeBuilder
  , toEscapedBuilder
  , toByteString
  , toChars
  --
  , addSlice
  --
  , ascii
  , asciiExp
  --
  , fromFinal
  , fromAddr
  , fromChars
  --
  , join
  , split
  --
  , writeChars
  , getWidths
  --
  , equal
  , compareLexical
  , compareFast
  )
  where


import Prelude hiding (String)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Internal as B
import qualified Data.ByteString.Internal as BS
import qualified Data.Char as Char
import qualified Data.List as List
import GHC.Exts (Char(C#), Int(I#), isTrue#)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(PlainPtr))
import GHC.IO (IO(IO))
import GHC.Prim
import GHC.Ptr (Ptr(Ptr))
import GHC.ST (ST(ST), runST)
import GHC.Word (Word8)
import Language.Haskell.TH (Q, Exp(AppE, LitE, VarE), Lit(BytesPrimL, IntPrimL))
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Bytes(..))

import qualified Bytes



-- BYTES


data String =
  String ByteArray#



-- EMPTY


empty :: String
empty =
  runST (ST (\s0 ->
    case newByteArray# 0#           s0 of { (# s1, mba #) ->
    case unsafeFreezeByteArray# mba s1 of { (# s2, ba  #) -> (# s2, String ba #) }}
  ))


isEmpty :: String -> Bool
isEmpty (String ba) =
  isTrue# (sizeofByteArray# ba ==# 0#)



-- SIZE


{-# INLINE size #-}
size :: String -> Int
size (String ba) =
  I# (sizeofByteArray# ba)


{-# INLINE size# #-}
size# :: String -> Int#
size# (String ba) =
  sizeofByteArray# ba



-- TO BUILDER


{-# INLINE toBuilder #-}
toBuilder :: String -> B.Builder
toBuilder =
  \(String ba) -> B.builder (toBuilderHelp 0# (sizeofByteArray# ba) ba)


{-# INLINE toRangeBuilder #-}
toRangeBuilder :: Int# -> Int# -> ByteArray# -> B.Builder
toRangeBuilder start end ba =
  B.builder (toBuilderHelp start (end -# start) ba)


{-# INLINE toBuilderHelp #-}
toBuilderHelp :: Int# -> Int# -> ByteArray# -> B.BuildStep a -> B.BuildStep a
toBuilderHelp start length0 ba k =
    go start length0
  where
    go i len (B.BufferRange (Ptr pos) (Ptr end)) =
      let
        !space = minusAddr# end pos
      in
      if isTrue# (len <=# space) then
        do  copyToAddr ba i pos len
            let !br = B.BufferRange (Ptr (plusAddr# pos len)) (Ptr end)
            k br
      else
        do  copyToAddr ba i pos space
            return $ B.bufferFull 1 (Ptr end) (go (i +# space) (len -# space))


{-# INLINE copyToAddr #-}
copyToAddr :: ByteArray# -> Int# -> Addr# -> Int# -> IO ()
copyToAddr ba i addr len =
  IO $ \s0 ->
    case copyByteArrayToAddr# ba i addr len s0 of
      s1 -> (# s1, () #)



-- TO ESCAPED BUILDER


{-# INLINE toEscapedBuilder #-}
toEscapedBuilder :: Word8# -> Word8# -> String -> B.Builder
toEscapedBuilder before after =
  \name -> B.builder (toEscapedBuilderHelp before after name)


{-# INLINE toEscapedBuilderHelp #-}
toEscapedBuilderHelp :: Word8# -> Word8# -> String -> B.BuildStep a -> B.BuildStep a
toEscapedBuilderHelp before after (String ba) k =
    go 0# (sizeofByteArray# ba)
  where
    go off want (B.BufferRange (Ptr pos) (Ptr end)) =
      let
        !have = minusAddr# end pos
      in
      if isTrue# (want <=# have) then
        do  -- PERF test if writing word-by-word is faster
            copyToAddr ba off pos want
            escape pos (plusAddr# pos want)
            let !newBufferRange = B.BufferRange (Ptr (plusAddr# pos want)) (Ptr end)
            k newBufferRange
      else
        do  copyToAddr ba off pos have
            escape pos (plusAddr# pos have)
            return $ B.bufferFull 1 (Ptr end) (go (off +# have) (want -# have))

    escape pos end =
      if isTrue# (ltAddr# pos end)
      then
        if isTrue# (eqWord8# before (indexWord8OffAddr# pos 0#))
        then
          do  IO $ \s0 -> case writeWord8OffAddr# pos 0# after s0 of { s1 -> (# s1, () #) }
              escape (plusAddr# pos 1#) end
        else
          do  escape (plusAddr# pos 1#) end
      else
        return ()



-- FROM PTR


{-# INLINE fromAddr #-}
fromAddr :: Addr# -> Addr# -> IO String
fromAddr pos end =
  IO $ \s0 ->
    let
      !len = minusAddr# end pos
    in
    case newByteArray# len s0                   of { (# s1, mba #) ->
    case copyAddrToByteArray# pos mba 0# len s1 of {    s2         ->
    case unsafeFreezeByteArray# mba s2          of { (# s3, ba  #) -> (# s3, String ba #) }}}



-- ASCII LITERALS
--
-- PERF revisit periodically to see if copyAddrToByteArray# can be skipped
-- because new primitives got added to Template Haskell.


ascii :: QuasiQuoter
ascii =
  QuasiQuoter
    { quoteExp  = asciiExp
    , quotePat  = \_ -> fail "cannot use [ascii| ... |] as a pattern"
    , quoteType = \_ -> fail "cannot use [ascii| ... |] as a type"
    , quoteDec  = \_ -> fail "cannot use [ascii| ... |] as a declaration"
    }


asciiExp :: [Char] -> Q Exp
asciiExp chars =
  do  bytes <- traverse toAsciiByte chars
      let (BS.PS fptr off len) = BS.pack bytes
      pure $
        VarE 'fromFinal
          `AppE` LitE (BytesPrimL (Bytes fptr (fromIntegral off) (fromIntegral len)))
          `AppE` LitE (IntPrimL (fromIntegral len))


toAsciiByte :: Char -> Q Word8
toAsciiByte char =
  let
    !word = Char.ord char
  in
  if word < 128
  then pure $ fromIntegral word
  else
    fail $
      "character " ++ show char ++ " is not ASCII, use utf8 QuasiQuoter instead!"


fromFinal :: Addr# -> Int# -> String
fromFinal addr len =
  runST (ST (\s0 ->
    case newByteArray# len                    s0 of { (# s1, mba #) ->
    case copyAddrToByteArray# addr mba 0# len s1 of {    s2         ->
    case unsafeFreezeByteArray# mba           s2 of { (# s3, ba  #) -> (# s3, String ba #) }}}
  ))



-- FROM CHARS


fromChars :: [Char] -> String
fromChars chars0 =
  runST (ST (\s0 ->
    let
      !(I# len) = getWidths chars0
    in
    case newByteArray# len          s0 of { (# s1, mba #) ->
    case loop mba 0# chars0         s1 of {    s2         ->
    case unsafeFreezeByteArray# mba s2 of { (# s3, ba  #) -> (# s3, String ba #) }}}
  ))
  where
    loop mba i chars s0 =
      case chars of
        [] ->
          s0

        (C# c):cs ->
          let
            !w = int2Word# (ord# c)
          in
          if isTrue# (ltWord# w 0x80##) then
              case writeWord8Array# mba i (wordToWord8# w) s0 of { s1 ->
                  loop mba (i +# 1#) cs s1
              }

          else if isTrue# (ltWord# w 0x800##) then
              case writeWord8Array# mba (i      ) (wordToWord8# (or# 0xC0## (            (uncheckedShiftRL# w 6#)))) s0 of { s1 ->
              case writeWord8Array# mba (i +# 1#) (wordToWord8# (or# 0x80## (and# 0x3F## (                  w   )))) s1 of { s2 ->
                  loop mba (i +# 2#) cs s2
              }}

          else if isTrue# (ltWord# w 0x10000##) then
              case writeWord8Array# mba (i      ) (wordToWord8# (or# 0xE0## (            (uncheckedShiftRL# w 12#)))) s0 of { s1 ->
              case writeWord8Array# mba (i +# 1#) (wordToWord8# (or# 0x80## (and# 0x3F## (uncheckedShiftRL# w  6#)))) s1 of { s2 ->
              case writeWord8Array# mba (i +# 2#) (wordToWord8# (or# 0x80## (and# 0x3F## (                  w    )))) s2 of { s3 ->
                  loop mba (i +# 3#) cs s3
              }}}

          else
              case writeWord8Array# mba (i      ) (wordToWord8# (or# 0xF0## (            (uncheckedShiftRL# w 18#)))) s0 of { s1 ->
              case writeWord8Array# mba (i +# 1#) (wordToWord8# (or# 0x80## (and# 0x3F## (uncheckedShiftRL# w 12#)))) s1 of { s2 ->
              case writeWord8Array# mba (i +# 2#) (wordToWord8# (or# 0x80## (and# 0x3F## (uncheckedShiftRL# w  6#)))) s2 of { s3 ->
              case writeWord8Array# mba (i +# 3#) (wordToWord8# (or# 0x80## (and# 0x3F## (                  w    )))) s3 of { s4 ->
                  loop mba (i +# 4#) cs s4
              }}}}



-- JOIN


{-# INLINE join #-}
join :: String -> Word8# -> String -> String
join (String ba1) w (String ba2) =
  runST $ ST $ \s0 ->
    case newByteArray# (len1 +# 1# +# len2)          s0 of { (# s1, mba #) ->
    case copyByteArray# ba1 0# mba 0# len1           s1 of {    s2         ->
    case writeWord8Array# mba len1 w                 s2 of {    s3         ->
    case copyByteArray# ba2 0# mba (len1 +# 1#) len2 s3 of {    s4         ->
    case unsafeFreezeByteArray# mba                  s4 of { (# s5, ba  #) ->
      (# s5, String ba #)
    }}}}}
  where
    !len1 = sizeofByteArray# ba1
    !len2 = sizeofByteArray# ba2



-- SPLIT


split :: Word8# -> String -> [String]
split sep (String ba) =
    go 0# (findDividers sep ba 0# len [])
  where
    !len = sizeofByteArray# ba

    go start offsets =
      case offsets of
        []        -> [ unsafeSlice ba start len ]
        I# o : os -> unsafeSlice ba start o : go (o +# 1#) os


findDividers :: Word8# -> ByteArray# -> Int# -> Int# -> [Int] -> [Int]
findDividers sep ba i len revOffsets =
  if isTrue# (i <# len) then
    findDividers sep ba (i +# 1#) len $
      if isTrue# (eqWord8# sep (indexWord8Array# ba i))
      then I# i : revOffsets
      else revOffsets
  else
    reverse revOffsets


unsafeSlice :: ByteArray# -> Int# -> Int# -> String
unsafeSlice ba start end =
  if isTrue# (start ==# end)
  then empty
  else
    runST $ ST $ \s0 ->
      case newByteArray# len                  s0 of { (# s1, mba #) ->
      case copyByteArray# ba start mba 0# len s1 of {    s2         ->
      case unsafeFreezeByteArray# mba         s2 of { (# s3, str #) ->
        (# s3, String str #)
      }}}
  where
    !len = end -# start



-- WRITE CHARS


writeChars :: Addr# -> [Char] -> IO ()
writeChars addr0 chars0 =
  IO $ \s0 ->
    case loop addr0 chars0 s0 of
      s1 -> (# s1, () #)
  where
    loop addr chars s0 =
      case chars of
        [] ->
          s0

        (C# c):cs ->
          let
            !w = int2Word# (ord# c)
          in
          if isTrue# (ltWord# w 0x80##) then
              case writeWord8OffAddr# addr 0# (wordToWord8# w) s0 of { s1 ->
                  loop (plusAddr# addr 1#) cs s1
              }

          else if isTrue# (ltWord# w 0x800##) then
              case writeWord8OffAddr# addr 0# (wordToWord8# (or# 0xC0## (            (uncheckedShiftRL# w 6#)))) s0 of { s1 ->
              case writeWord8OffAddr# addr 1# (wordToWord8# (or# 0x80## (and# 0x3F## (                  w   )))) s1 of { s2 ->
                  loop (plusAddr# addr 2#) cs s2
              }}

          else if isTrue# (ltWord# w 0x10000##) then
              case writeWord8OffAddr# addr 0# (wordToWord8# (or# 0xE0## (            (uncheckedShiftRL# w 12#)))) s0 of { s1 ->
              case writeWord8OffAddr# addr 1# (wordToWord8# (or# 0x80## (and# 0x3F## (uncheckedShiftRL# w  6#)))) s1 of { s2 ->
              case writeWord8OffAddr# addr 2# (wordToWord8# (or# 0x80## (and# 0x3F## (                  w    )))) s2 of { s3 ->
                  loop (plusAddr# addr 3#) cs s3
              }}}

          else
              case writeWord8OffAddr# addr 0# (wordToWord8# (or# 0xF0## (            (uncheckedShiftRL# w 18#)))) s0 of { s1 ->
              case writeWord8OffAddr# addr 1# (wordToWord8# (or# 0x80## (and# 0x3F## (uncheckedShiftRL# w 12#)))) s1 of { s2 ->
              case writeWord8OffAddr# addr 2# (wordToWord8# (or# 0x80## (and# 0x3F## (uncheckedShiftRL# w  6#)))) s2 of { s3 ->
              case writeWord8OffAddr# addr 3# (wordToWord8# (or# 0x80## (and# 0x3F## (                  w    )))) s3 of { s4 ->
                  loop (plusAddr# addr 4#) cs s4
              }}}}



-- GET WIDTHS


getWidths :: [Char] -> Int
getWidths chars =
  List.sum (List.map getWidth chars)


{-# INLINE getWidth #-}
getWidth :: Char -> Int
getWidth char
  | code < 0x80    = 1
  | code < 0x800   = 2
  | code < 0x10000 = 3
  | otherwise      = 4
  where
    code = Char.ord char



-- INSTANCES


instance Eq String where
  (==) = equal

instance Ord String where
  compare = compareFast


{-# INLINE equal #-}
equal :: String -> String -> Bool
equal (String ba1) (String ba2) =
  Bytes.equal ba1 ba2


{-# INLINE compareLexical #-}
compareLexical :: String -> String -> Ordering
compareLexical (String ba1) (String ba2) =
  Bytes.compareLexical ba1 ba2


{-# INLINE compareFast #-}
compareFast :: String -> String -> Ordering
compareFast (String ba1) (String ba2) =
  Bytes.compareFast ba1 ba2



-- TO BYTESTRING


toByteString :: String -> IO BS.ByteString
toByteString (String ba) =
  IO $ \s0 ->
    case newPinnedByteArray#         len s0 of { (# s1, mba #) ->
    case copyByteArray# ba 0# mba 0# len s1 of {    s2         ->
    case mutableByteArrayContents# mba      of {        addr   ->
      (# s2, BS.BS (ForeignPtr addr (PlainPtr mba)) (I# len) #)
    }}}
  where
    !len = sizeofByteArray# ba



-- TO CHARS


toChars :: String -> [Char]
toChars (String ba) =
  addSlice ba 0# (sizeofByteArray# ba) []



-- ADD SLICE


addSlice :: ByteArray# -> Int# -> Int# -> [Char] -> [Char]
addSlice ba offset len chars =
    go offset
  where
    go i =
      if isTrue# (i >=# len)
      then chars
      else
        let
          !w = getWord8 ba i
          !(# char, width #)
            | isTrue# (ltWord# w 0xC0##) = (# C# (chr# (word2Int# w)), 1# #)
            | isTrue# (ltWord# w 0xE0##) = (# chr2 ba i w, 2# #)
            | isTrue# (ltWord# w 0xF0##) = (# chr3 ba i w, 3# #)
            | True                       = (# chr4 ba i w, 4# #)
        in
        char : go (i +# width)


{-# INLINE chr2 #-}
chr2 :: ByteArray# -> Int# -> Word# -> Char
chr2 ba i w1 =
  let
    !w2 = getWord8 ba (i +# 1#)
    !c1 = uncheckedShiftL# (and# 0x3F## w1) 6#
    !c2 = and# 0x3F## w2
  in
  C# (chr# (word2Int# (or# c1 c2)))


{-# INLINE chr3 #-}
chr3 :: ByteArray# -> Int# -> Word# -> Char
chr3 ba i w1 =
  let
    !w2 = getWord8 ba (i +# 1#)
    !w3 = getWord8 ba (i +# 2#)
    !c1 = uncheckedShiftL# (and# 0x0F## w1) 12#
    !c2 = uncheckedShiftL# (and# 0x3F## w2) 6#
    !c3 = and# 0x3F## w3
  in
  C# (chr# (word2Int# (or# c1 (or# c2 c3))))


{-# INLINE chr4 #-}
chr4 :: ByteArray# -> Int# -> Word# -> Char
chr4 ba i w1 =
  let
    !w2 = getWord8 ba (i +# 1#)
    !w3 = getWord8 ba (i +# 2#)
    !w4 = getWord8 ba (i +# 3#)
    !c1 = uncheckedShiftL# (and# 0x07## w1) 18#
    !c2 = uncheckedShiftL# (and# 0x3F## w2) 12#
    !c3 = uncheckedShiftL# (and# 0x3F## w3) 6#
    !c4 = and# 0x3F## w4
  in
  C# (chr# (word2Int# (or# (or# c1 c2) (or# c3 c4))))


{-# INLINE getWord8 #-}
getWord8 :: ByteArray# -> Int# -> Word#
getWord8 ba i =
  word8ToWord# (indexWord8Array# ba i)

