{-# LANGUAGE BangPatterns, BinaryLiterals, ExtendedLiterals, MagicHash,
TemplateHaskell, UnboxedTuples
#-}
module AST.Prim.Tag
  ( Tag
  , zero
  --
  , cons
  , nil
  --
  , toBuilder
  , toWord32
  --
  , Size(..)
  , toSize
  , toWidth
  --
  , read
  , addSize
  --
  , Table(..)
  , table
  , tagSize
  , get
  , map
  , mapWithTag
  , foldl
  , foldlWithTag
  , mmap
  , mmap1
  , mmapWithTag
  , mtraverse
  , mtraverseWithTag
  , traverse_
  , traverseWithTag
  , traverseMaybe
  , toDict
  , unpack
  --
  , Sample(..)
  , sample
  --
  , eTag, dTag
  , eSize, dSize
  , eTable, dTable
  )
  where


import Prelude hiding (map, foldl, read)
import Data.Bits ((.&.), unsafeShiftR)
import Data.Functor.Classes (Eq1, liftEq)
import qualified Data.ByteString.Builder as B
import qualified Data.List as List
import qualified Data.Map as Map
import GHC.Exts (isTrue#)
import GHC.Int (Int(..))
import GHC.Prim
import GHC.ST (ST(ST), runST)
import GHC.Word (Word8(..), Word16(..), Word32(..))

import qualified Array as A
import qualified Bytes
import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Crash



-- TAG
--
-- Tags for each variant of a custom type.
--
-- The lowest two bits indicate how many bytes are needed for on-disk storage:
--     00 = 1 byte
--     01 = 2 bytes
--     11 = 4 bytes
--


newtype Tag = Tag Word32
  deriving (Eq, Ord)


zero :: Tag
zero =
  Tag 0


cons :: Tag; cons = addSize One 0
nil  :: Tag; nil  = addSize One 1



-- TO BUILDER


toBuilder :: Tag -> B.Builder
toBuilder tag =
  B.word32Dec (toWord32 tag)


toWord32 :: Tag -> Word32
toWord32 (Tag tag) =
  unsafeShiftR tag 2



-- SIZE


data Size
  = One
  | Two
  | Four
  deriving (Eq, Ord)


toSize :: Tag -> Size
toSize (Tag tag) =
  case tag .&. 0b11 of
    0 -> One
    1 -> Two
    _ -> Four


toWidth :: Size -> Word
toWidth size =
  case size of
    One  -> 1
    Two  -> 2
    Four -> 4


read :: Size -> Addr# -> IO Tag
read size addr =
  case size of
    One  -> addSize# 0b00#Word32 . fromIntegral <$> Bytes.getWord8    W8#   addr
    Two  -> addSize# 0b01#Word32 . fromIntegral <$> Bytes.getWord16_LE W16# addr
    Four -> addSize# 0b11#Word32                <$> Bytes.getWord32_LE W32# addr


addSize :: Size -> Word32 -> Tag
addSize size n =
  addSize#
  (
    case size of
      One  -> 0b00#Word32
      Two  -> 0b01#Word32
      Four -> 0b11#Word32
  )
  n


addSize# :: Word32# -> Word32 -> Tag
addSize# code (W32# n) =
  Tag (W32# (uncheckedShiftLWord32# n 2# `orWord32#` code))



--------------------------------------------------------------------------------
-- TABLE -----------------------------------------------------------------------
--------------------------------------------------------------------------------


data Table a =
  Table (SmallArray# a)

instance Eq a => Eq (Table a) where
  (==) = equalHelp (==)

instance Eq1 Table where
  liftEq = equalHelp


equalHelp :: (a -> b -> Bool) -> Table a -> Table b -> Bool
equalHelp eq (Table sa1) (Table sa2) =
    isTrue# (len1 ==# len2) && go 0#
  where
    !len1 = sizeofSmallArray# sa1
    !len2 = sizeofSmallArray# sa2

    go i =
      if isTrue# (i <# len1)
      then
        case indexSmallArray# sa1 i of { (# x #) ->
        case indexSmallArray# sa2 i of { (# y #) -> eq x y && go (i +# 1#) }}
      else
        True



-- CREATE TABLE


table :: a -> a -> [a] -> Table a
table x y zs =
  runST $ ST $ \s0 ->
    let
      !(I# len) = 2 + List.length zs
    in
    case newSmallArray# len x        s0 of { (# s1, sma #) ->
    case writeSmallArray# sma 1# y   s1 of {    s2         ->
    case tableHelp sma 2# zs         s2 of {    s3         ->
    case unsafeFreezeSmallArray# sma s3 of { (# s4, sa  #) ->
      (# s4, Table sa #)
    }}}}


tableHelp :: SmallMutableArray# s a -> Int# -> [a] -> State# s -> State# s
tableHelp sma i list s0 =
  case list of
    [] ->
      s0

    x:xs ->
      case writeSmallArray# sma i x s0 of
        s1 ->
          tableHelp sma (i +# 1#) xs s1



-- TAG SIZE


tagSize :: Table a -> Size
tagSize (Table sa)
  | isTrue# (len <=# 0xFF#      ) = One
  | isTrue# (len <=# 0xFFFF#    ) = Two
  | isTrue# (len <=# 0x3FFFFFFF#) = Four
  | otherwise                     = $(Crash.crash 'tagSize) "compiler cannot handle custom types with more than 2^30 variants"
  where
    !len = sizeofSmallArray# sa



-- GET


get :: Tag -> Table a -> a
get (Tag (W32# w#)) (Table sa) =
  case indexSmallArray# sa (word2Int# (uncheckedShiftRL# (word32ToWord# w#) 2#)) of
    (# a #) ->
      a



-- MAP


map :: (a -> b) -> Table a -> Table b
map func (Table sa) =
  runST $ ST $ \s0 ->
    case indexSmallArray# sa 0#              of { (#     a   #) ->
    case indexSmallArray# sa 1#              of { (#     b   #) ->
    case newSmallArray#   len    (func a) s0 of { (# s1, sma #) ->
    case writeSmallArray# sma 1# (func b) s1 of {    s2         ->
    case loop 2# sma                      s2 of {    s3         ->
    case unsafeFreezeSmallArray# sma      s3 of { (# s4, sa' #) ->
      (# s4, Table sa' #)
    }}}}}}
  where
    !len = sizeofSmallArray# sa

    loop i sma s0 =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa  i             of { (# a #) ->
        case writeSmallArray# sma i (func a) s0 of {   s1    ->
          loop (i +# 1#) sma s1
        }}
      else
        s0



-- MAP WITH TAG


mapWithTag :: (Tag -> a -> b) -> Table a -> Table b
mapWithTag func (Table sa) =
  runST $ ST $ \s0 ->
    case indexSmallArray# sa 0#                              of { (#     a   #) ->
    case indexSmallArray# sa 1#                              of { (#     b   #) ->
    case newSmallArray#   len    (func (toTag code 0#) a) s0 of { (# s1, sma #) ->
    case writeSmallArray# sma 1# (func (toTag code 1#) b) s1 of {    s2         ->
    case loop 2# sma                                      s2 of {    s3         ->
    case unsafeFreezeSmallArray# sma                      s3 of { (# s4, sa' #) ->
      (# s4, Table sa' #)
    }}}}}}
  where
    !len  = sizeofSmallArray# sa
    !code = toCode len

    loop i sma s0 =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa  i                            of { (# a #) ->
        case writeSmallArray# sma i (func (toTag code i) a) s0 of {   s1    ->
          loop (i +# 1#) sma s1
        }}
      else
        s0



-- FOLDL


foldl :: (a -> s -> s) -> s -> Table a -> s
foldl step base (Table sa) =
    go 0# base
  where
    !len = sizeofSmallArray# sa

    go i state =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa i of
          (# value #) ->
            go (i +# 1#) (step value state)
      else
        state



-- FOLDL WITH TAG


foldlWithTag :: (Tag -> a -> s -> s) -> s -> Table a -> s
foldlWithTag step base (Table sa) =
    go 0# base
  where
    !len  = sizeofSmallArray# sa
    !code = toCode len

    go i state =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa i of
          (# value #) ->
            go (i +# 1#) (step (toTag code i) value state)
      else
        state


toCode :: Int# -> Word#
toCode len
  | isTrue# (len <=# 0xFF#      ) = 0b00##
  | isTrue# (len <=# 0xFFFF#    ) = 0b01##
  | isTrue# (len <=# 0x3FFFFFFF#) = 0b11##
  | otherwise                     = $(Crash.crash 'toCode) "compiler cannot handle custom types with more than 2^30 variants"


toTag :: Word# -> Int# -> Tag
toTag code i =
  Tag (W32# (wordToWord32# (or# code (uncheckedShiftL# (int2Word# i) 2#))))



-- MMAP


mmap :: (Monoid m) => (a -> m) -> Table a -> m
mmap func tbl =
  mmap1 func func tbl


mmap1 :: (Monoid m) => (a -> m) -> (a -> m) -> Table a -> m
mmap1 func1 funcN (Table sa) =
  case indexSmallArray# sa 0# of { (# x #) ->
  case indexSmallArray# sa 1# of { (# y #) ->
    func1 x <> funcN y <> loop 2#
  }}
  where
    !len = sizeofSmallArray# sa

    loop i =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa i of
          (# a #) ->
            funcN a <> loop (i +# 1#)
      else
        mempty



-- MMAP WITH TAG


mmapWithTag :: (Tag -> a -> B.Builder) -> Table a -> B.Builder
mmapWithTag func (Table sa) =
  case indexSmallArray# sa 0# of { (# x #) ->
  case indexSmallArray# sa 1# of { (# y #) ->
    func (toTag code 0#) x <> func (toTag code 1#) y <> loop 2#
  }}
  where
    !len  = sizeofSmallArray# sa
    !code = toCode len

    loop i =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa i of
          (# a #) ->
            func (toTag code i) a <> loop (i +# 1#)
      else
        mempty



-- MTRAVERSE


mtraverse :: (Applicative f) => (a -> f B.Builder) -> Table a -> f B.Builder
mtraverse func (Table sa) =
  case indexSmallArray# sa 0# of
    (# x #) ->
      mappend <$> func x <*>
        case indexSmallArray# sa 1# of
          (# y #) ->
            mappend <$> func y <*> go 2#
  where
    !len = sizeofSmallArray# sa

    go i =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa i of
          (# a #) ->
            mappend <$> func a <*> go (i +# 1#)
      else
        pure mempty



-- MTRAVERSE WITH TAG


mtraverseWithTag :: (Applicative f) => (Tag -> a -> f B.Builder) -> Table a -> f B.Builder
mtraverseWithTag func (Table sa) =
  case indexSmallArray# sa 0# of
    (# x #) ->
      mappend <$> func (toTag code 0#) x <*>
        case indexSmallArray# sa 1# of
          (# y #) ->
            mappend <$> func (toTag code 1#) y <*> go 2#
  where
    !len  = sizeofSmallArray# sa
    !code = toCode len

    go i =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa i of
          (# a #) ->
            mappend <$> func (toTag code i) a <*> go (i +# 1#)
      else
        pure mempty



-- TRAVERSE_


traverse_ :: (Applicative f) => (a -> f b) -> Table a -> f ()
traverse_ func (Table sa) =
  case indexSmallArray# sa 0# of { (# a #) ->
  case indexSmallArray# sa 1# of { (# b #) ->
    func a *> func b *> loop 2#
  }}
  where
    !len = sizeofSmallArray# sa

    loop i =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa i of
          (# v #) ->
            func v *> loop (i +# 1#)
      else
        pure ()



-- TRAVERSE WITH TAG


traverseWithTag :: (Applicative f) => (Tag -> a -> f b) -> Table a -> f (Table b)
traverseWithTag func (Table sa) =
    mkTable <$> mkList
  where
    !len  = sizeofSmallArray# sa
    !code = toCode len

    mkTable list =
      runST $ ST $ \s0 ->
        case newSmallArray# len $(Crash.blank 'traverseWithTag) s0 of { (# s1, sma #) ->
        case loop sma 0# list                                   s1 of {    s2         ->
        case unsafeFreezeSmallArray# sma                        s2 of { (# s3, tbl #) ->
          (# s3, Table tbl #)
        }}}
      where
        loop sma i values s0 =
          case values of
            []   -> s0
            v:vs ->
              case writeSmallArray# sma i v s0 of
                s1 -> loop sma (i +# 1#) vs s1

    mkList =
      case indexSmallArray# sa 0# of { (# a #) ->
      case indexSmallArray# sa 1# of { (# b #) ->
        liftA2 (:) (func (toTag code 0#) a) $
        liftA2 (:) (func (toTag code 1#) b) $ loop 2#
      }}
      where
        loop i =
          if isTrue# (i <# len)
          then
            case indexSmallArray# sa i of
              (# v #) ->
                liftA2 (:) (func (toTag code i) v) $ loop (i +# 1#)
          else
            pure []



-- TRAVERSE MAYBE


traverseMaybe :: (a -> Maybe b) -> Table a -> Maybe (Table b)
traverseMaybe func (Table sa) =
  let
    !len = sizeofSmallArray# sa
  in
  case indexSmallArray# sa 0# of
    (# a #) ->
      do  x <- func a
          case indexSmallArray# sa 1# of
            (# b #) ->
              do  y <- func b
                  runST $ ST $ \s0 ->
                    case newSmallArray# len x      s0 of { (# s1, sma #) ->
                    case writeSmallArray# sma 1# y s1 of {    s2         ->
                      traverseMaybe# sa sma func len s2
                    }}


{-# INLINE traverseMaybe# #-}
traverseMaybe# :: SmallArray# a -> SmallMutableArray# s b -> (a -> Maybe b) -> Int# -> State# s -> (# State# s, Maybe (Table b) #)
traverseMaybe# sa sma func len s =
    go 2# s
  where
    go i s0 =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa i of
          (# a #) ->
            case func a of
              Just b ->
                case writeSmallArray# sma i b s0 of
                  s1 ->
                    go (i +# 1#) s1

              Nothing ->
                (# s0, Nothing #)
      else
        case unsafeFreezeSmallArray# sma s0 of
          (# s1, sa' #) ->
            (# s1, Just (Table sa') #)



-- TO DICT


toDict :: (Ord k) => Table a -> (Tag -> a -> (k,v)) -> Map.Map k v
toDict (Table sa) func =
    go 0# []
  where
    !len  = sizeofSmallArray# sa
    !code = toCode len

    go i pairs =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa i of
          (# value #) ->
            let
              !tag  = toTag code i
              !pair = func tag value
            in
            go (i +# 1#) (pair:pairs)
      else
        Map.fromList pairs



-- UNPACK


unpack :: Table a -> (Int -> [b] -> r) -> (Tag -> a -> b) -> r
unpack (Table sa) done func =
    go (len -# 1#) []
  where
    !len  = sizeofSmallArray# sa
    !code = toCode len

    go i bs =
      if isTrue# (i >=# 0#)
      then
        case indexSmallArray# sa i of
          (# a #) ->
            let
              !b = func (toTag code i) a
            in
            go (i -# 1#) (b:bs)
      else
        done (I# len) bs



-- SAMPLE


data Sample a
  = Sample2 a a
  | Sample3 a a a
  | SampleN a a


sample :: Table a -> Sample a
sample (Table sa)
  | isTrue# (len ==# 2#) =
      case indexSmallArray# sa 0# of
        (# a #) ->
          case indexSmallArray# sa 1# of
            (# b #) ->
              Sample2 a b

  | isTrue# (len ==# 3#) =
      case indexSmallArray# sa 0# of
        (# a #) ->
          case indexSmallArray# sa 1# of
            (# b #) ->
              case indexSmallArray# sa 1# of
                (# c #) ->
                  Sample3 a b c

  | otherwise =
      case indexSmallArray# sa 0# of
        (# a #) ->
          case indexSmallArray# sa 1# of
            (# b #) ->
              SampleN a b
  where
    !len = sizeofSmallArray# sa



-- ENCODE/DECODE


eTag :: Tag -> B.Builder
eTag (Tag tag) =
  E.u32 tag


dTag :: D.Decoder Tag
dTag =
  Tag <$> D.u32


eSize :: Size -> B.Builder
eSize size =
  case size of
    One  -> E.u8 0
    Two  -> E.u8 1
    Four -> E.u8 2


dSize :: D.Decoder Size
dSize =
  do  tag <- D.u8
      case tag of
        0 -> pure One
        1 -> pure Two
        2 -> pure Four
        _ -> D.expecting "Tag.Size"


eTable :: (a -> B.Builder) -> Table a -> B.Builder
eTable enc (Table sa) =
  E.array32 enc (A.Array sa)


dTable :: D.Decoder a -> D.Decoder (Table a)
dTable dec =
  do  (A.Array sa) <- D.array32 dec
      return (Table sa)

