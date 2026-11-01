{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, TemplateHaskell, UnboxedTuples #-}
module AST.Prim.Fields
  ( Fields(..)
  --
  , Order#
  , Order(..)
  , unordered
  , bestOrder
  --
  , size
  , map
  , mapWithKey
  , mmap
  , mmapWithKey
  , traverse
  , traverse_
  , traverseWithKey
  , mtraverseWithKey
  , mindexedTraverse
  , mindexedTraverseWithKey
  , foldr
  , foldrWithKey
  , foldrWithKeyM
  , all
  , any
  , extend
  --
  , toNEM
  --
  , Ordered(..)
  , fromOrdered
  , fromUnordered
  --
  , flatten
  , flattenKeys
  , flattenValues
  , flattenWith
  --
  , encode
  , decode
  )
  where


import Prelude hiding (any, all, foldr, map, traverse)
import Data.Functor.Classes (Eq1, liftEq)
import qualified Data.NonEmptyMap as NEM
import GHC.Exts (isTrue#)
import GHC.Int (Int(..))
import GHC.Prim
import GHC.ST (ST(ST), runST)

import qualified Bytes
import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Crash

import qualified AST.Prim.Name as N



-- FIELDS
--
-- Store record fields for O(log(n)) access, but also track the source order
-- of the fields so that they can be flattened into a list in O(n) in the same
-- order they appeared in a type alias or type annotation.


data Fields a =
  Fields Order# (NEM.Map N.Name a)



-- ORDER
--
-- The order values are stored as Word8, Word16, or Word32 depending on the
-- number of record fields. The order size can be determined by inspecting the
-- length of Order#


type Order# = ByteArray#


data Order =
  Order ByteArray#


{-# NOINLINE unordered #-}
unordered :: Order
unordered =
  runST $ ST $ \s0 ->
    case newByteArray# 0#           s0 of { (# s1, mba #) ->
    case unsafeFreezeByteArray# mba s1 of { (# s2, ba  #) ->
      (# s2, Order ba #)
    }}


bestOrder :: Order# -> Order# -> Order#
bestOrder order1 order2 =
  if isTrue# (sizeofByteArray# order1 >=# sizeofByteArray# order2)
  then order1
  else order2



-- HELPERS


size :: Fields a -> Int
size (Fields _ dict) =
  NEM.size dict


map :: (a -> b) -> Fields a -> Fields b
map func (Fields order dict) =
  Fields order (NEM.map func dict)


mapWithKey :: (N.Name -> a -> b) -> Fields a -> Fields b
mapWithKey func (Fields order dict) =
  Fields order (NEM.mapWithKey func dict)


mmap :: (Monoid m) => (a -> m) -> Fields a -> m
mmap func (Fields _ dict) =
  NEM.mmap func dict


mmapWithKey :: (Monoid m) => (N.Name -> a -> m) -> Fields a -> m
mmapWithKey func (Fields _ dict) =
  NEM.mmapWithKey func dict


traverse :: (Applicative f) => (a -> f b) -> Fields a -> f (Fields b)
traverse func (Fields order dict) =
  Fields order <$> NEM.traverse func dict


traverse_ :: (Applicative f) => (a -> f ()) -> Fields a -> f ()
traverse_ func (Fields _ dict) =
  NEM.traverse_ func dict


traverseWithKey :: (Applicative f) => (N.Name -> a -> f b) -> Fields a -> f (Fields b)
traverseWithKey func (Fields order dict) =
  Fields order <$> NEM.traverseWithKey func dict


mtraverseWithKey :: (Applicative f, Monoid m) => (N.Name -> a -> f m) -> Fields a -> f m
mtraverseWithKey func (Fields _ dict) =
  NEM.mtraverseWithKey func dict


mindexedTraverse :: (Applicative f, Monoid m) => (Int -> a -> f m) -> Fields a -> f m
mindexedTraverse func (Fields _ dict) =
  NEM.mindexedTraverse func dict


mindexedTraverseWithKey :: (Applicative f, Monoid m) => (Int -> N.Name -> a -> f m) -> Fields a -> f m
mindexedTraverseWithKey func (Fields _ dict) =
  NEM.mindexedTraverseWithKey func dict


foldr :: (a -> s -> s) -> s -> Fields a -> s
foldr step base (Fields _ dict) =
  NEM.foldr step base dict


foldrWithKey :: (N.Name -> a -> s -> s) -> s -> Fields a -> s
foldrWithKey step base (Fields _ dict) =
  NEM.foldrWithKey step base dict


foldrWithKeyM :: (Monad m) => (N.Name -> a -> s -> m s) -> s -> Fields a -> m s
foldrWithKeyM step base (Fields _ dict) =
  NEM.foldrWithKeyM step base dict



-- BOOL HELPERS


all :: (a -> Bool) -> Fields a -> Bool
all isGood (Fields _ dict) =
  NEM.all isGood dict


any :: (a -> Bool) -> Fields a -> Bool
any isGood (Fields _ dict) =
  NEM.any isGood dict



-- IDEA try to merge the orders?
--
extend :: Fields a -> Fields a -> Fields a
extend (Fields _ olds) (Fields _ news) =
    Fields order (NEM.union olds news)
  where
    !(Order order) = unordered



-- TO DICT


toNEM :: Fields a -> NEM.Map N.Name a
toNEM (Fields _ dict) =
  dict



-- FROM DICT


data Ordered a =
  Ordered
    { _index :: Int
    , _value :: a
    }


fromOrdered :: (Applicative f) => (a -> f b) -> NEM.Map N.Name (Ordered a) -> f (Fields b)
fromOrdered func dict =
  runST $ ST $ \s0 ->
    case newByteArray# len                     s0 of { (# s1, mba   #) ->
    case NEM.indexedTraverse# (write mba) dict s1 of {    s2           ->
    case unsafeFreezeByteArray# mba            s2 of { (# s3, order #) ->
      (# s3, Fields order <$> NEM.traverse (func . _value) dict #)
    }}}
  where
    !(I# num) = NEM.size dict

    !len
      | isTrue# (num <=# 0x100#  ) =       num
      | isTrue# (num <=# 0x10000#) = 2# *# num
      | otherwise                  = 4# *# num

    !write
      | isTrue# (num <=# 0x100#  ) = write8
      | isTrue# (num <=# 0x10000#) = write16
      | otherwise                  = write32

    write8  mba i (Ordered (I# j) _) s = writeWord8Array#  mba i (wordToWord8#  (int2Word# j)) s
    write16 mba i (Ordered (I# j) _) s = writeWord16Array# mba i (wordToWord16# (int2Word# j)) s
    write32 mba i (Ordered (I# j) _) s = writeWord32Array# mba i (wordToWord32# (int2Word# j)) s


fromUnordered :: NEM.Map N.Name a -> Fields a
fromUnordered dict =
    Fields order dict
  where
    !(Order order) = unordered



-- INSTANCES


instance Eq a => Eq (Fields a) where
  (==) (Fields o1 d1) (Fields o2 d2) =
    Bytes.equal o1 o2 && d1 == d2


instance Eq1 Fields where
  liftEq eq (Fields o1 d1) (Fields o2 d2) =
    Bytes.equal o1 o2 && liftEq eq d1 d2


instance Ord a => Ord (Fields a) where
  compare (Fields o1 d1) (Fields o2 d2) =
    let
      !len1 = sizeofByteArray# o1
      !len2 = sizeofByteArray# o2
    in
    if isTrue# (len1 <# len2) then LT else
    if isTrue# (len1 ># len2) then GT else
      case compare d1 d2 of
        EQ ->
          let !cmp# = compareByteArrays# o1 0# o2 0# len1 in
          if isTrue# (cmp# <# 0#) then LT else
          if isTrue# (cmp# ># 0#) then GT else EQ

        ne ->
          ne



-- FLATTEN


type List1 a = (a, [a])
type Pair  a = (N.Name, a)


{-# INLINE flatten       #-}
{-# INLINE flattenKeys   #-}
{-# INLINE flattenValues #-}

flatten       :: Fields a -> List1 (Pair a)
flattenKeys   :: Fields a -> List1 N.Name
flattenValues :: Fields a -> List1 a

flatten       = flattenWith (,)
flattenKeys   = flattenWith (\k _ -> k)
flattenValues = flattenWith (\_ v -> v)


flattenWith :: (N.Name -> a -> v) -> Fields a -> List1 v
flattenWith func (Fields order dict) =
  if isTrue# (sizeofByteArray# order /=# len)
  then NEM.toListWithNE func dict
  else
    runST $ ST $ \s0 ->
      case newSmallArray# num blank                     s0 of { (# s1, sma #) ->
      case NEM.indexedTraverseWithKey# (write sma) dict s1 of {    s2         ->
      case unsafeFreezeSmallArray# sma                  s2 of { (# s3, sa  #) ->
        (# s3, toList1 sa #)
      }}}
  where
    !(I# num) = NEM.size dict
    blank = $(Crash.crash 'flattenWith) "invalid record field order"

    len
      | isTrue# (num <=# 0x100#  ) =       num
      | isTrue# (num <=# 0x10000#) = 2# *# num
      | otherwise                  = 4# *# num

    get
      | isTrue# (num <=# 0x100#  ) = get8
      | isTrue# (num <=# 0x10000#) = get16
      | otherwise                  = get32

    get8  i = word2Int# (word8ToWord#  (indexWord8Array#  order i))
    get16 i = word2Int# (word16ToWord# (indexWord16Array# order i))
    get32 i = word2Int# (word32ToWord# (indexWord32Array# order i))

    write sma i k v s =
      writeSmallArray# sma (get i) (func k v) s


toList1 :: SmallArray# a -> List1 a
toList1 sa# =
    go (len# -# 1#) []
  where
    !len# = sizeofSmallArray# sa#

    go i# vs =
      case indexSmallArray# sa# i# of
        (# v #) ->
          if isTrue# (i# ==# 0#)
          then (v,vs)
          else go (i# -# 1#) (v:vs)



-- BINARY SERIALIZATION


encode :: (a -> E.Builder) -> Fields a -> E.Builder
encode eField (Fields order dict) =
  NEM.encode32 N.encode eField dict
  <> eOrder order dict


decode :: D.Decoder a -> D.Decoder (Fields a)
decode dField =
  do  dict <- NEM.decode32 N.decode dField
      dOrder dict



-- SERIALIZE ORDER


eOrder :: Order# -> NEM.Map k a -> E.Builder
eOrder o dict
  | num <= 0x100   = E.bytes     o
  | num <= 0x10000 = E.words16LE o
  | otherwise      = E.words32LE o
  where
    num = NEM.size dict


dOrder :: NEM.Map N.Name a -> D.Decoder (Fields a)
dOrder dict
  | isTrue# (num <=# 0x100#  ) = D.bytes#    (\o -> Fields o dict) num
  | isTrue# (num <=# 0x10000#) = D.words16LE (\o -> Fields o dict) num
  | otherwise                  = D.words32LE (\o -> Fields o dict) num
  where
    !(I# num) = NEM.size dict

