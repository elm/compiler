{-# LANGUAGE BangPatterns, MagicHash, Rank2Types, TemplateHaskell, UnboxedTuples #-}
module Array
  ( Array(..)
  , empty
  , singleton
  --
  , isEmpty
  , size
  , get
  --
  , map
  , mmap
  --
  , traverse_
  , traverseIO
  , traverseM
  , traverseMaybe
  , traverseEither
  --
  , foldl
  , foldr
  , foldlM
  , foldlMapIO
  --
  , toList
  , toListWith
  --
  , addToList
  --
  , fromList
  , fromList1
  , fromListWith
  , fromListWith1
  )
  where


import Prelude hiding (foldl, foldr, map)
import Data.Functor.Classes (Eq1, liftEq)
import qualified Data.List as List
import GHC.Exts (Int(..), isTrue#)
import GHC.IO (IO(IO))
import GHC.Prim
import GHC.ST (ST(ST), runST)

import qualified Crash



-- ARRAY


data Array a =
  Array (SmallArray# a)



-- EMPTY


empty :: Array a
empty =
  runST $ ST $ \s0 ->
    case newSmallArray# 0# $(Crash.blank 'empty) s0 of { (# s1, sma #) ->
    case unsafeFreezeSmallArray# sma             s1 of { (# s2, sa  #) -> (# s2, Array sa #) }}



-- SINGLETON


singleton :: a -> Array a
singleton a =
  runST $ ST $ \s0 ->
    case newSmallArray# 1# a         s0 of { (# s1, sma #) ->
    case unsafeFreezeSmallArray# sma s1 of { (# s2, sa  #) -> (# s2, Array sa #) }}



-- IS EMPTY


isEmpty :: Array a -> Bool
isEmpty (Array sa) =
  isTrue# (sizeofSmallArray# sa ==# 0#)



-- SIZE


size :: Array a -> Int
size (Array sa) =
  I# (sizeofSmallArray# sa)



-- GET


get :: Int -> Array a -> a
get (I# i) (Array sa) =
  case indexSmallArray# sa i of
    (# a #) ->
      a



-- MAP


map :: (a -> b) -> Array a -> Array b
map func (Array sa) =
  if isTrue# (len ==# 0#)
  then empty
  else
    runST $ ST $ \s0 ->
      case indexSmallArray# sa 0#         of { (#     a   #) ->
      case newSmallArray# len (func a) s0 of { (# s1, sma #) ->
      case map# func sa sma len        s1 of {    s2         ->
      case unsafeFreezeSmallArray# sma s2 of { (# s3, sa' #) -> (# s3, Array sa' #) }}}}
  where
    !len = sizeofSmallArray# sa


map# :: (a -> b) -> SmallArray# a -> SmallMutableArray# s b -> Int# -> State# s -> State# s
map# func sa sma len =
    loop 1#
  where
    loop i s0 =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa  i             of { (# a #) ->
        case writeSmallArray# sma i (func a) s0 of { s1 -> loop (i +# 1#) s1 }}
      else
        s0



-- MMAP


mmap :: (Monoid m) => (a -> m) -> Array a -> m
mmap func (Array array) =
    loop 0#
  where
    !len = sizeofSmallArray# array

    loop i =
      if isTrue# (i <# len)
      then case indexSmallArray# array i of { (# a #) -> func a <> loop (i +# 1#) }
      else mempty



-- TRAVERSE


traverse_ :: (Applicative f) => (a -> f b) -> Array a -> f ()
traverse_ func (Array sa) =
    loop 0#
  where
    !len = sizeofSmallArray# sa

    loop i =
      if isTrue# (i <# len)
      then case indexSmallArray# sa i of { (# a #) -> func a *> loop (i +# 1#) }
      else pure ()



-- TRAVERSE IO


traverseIO :: (a -> IO b) -> Array a -> IO (Array b)
traverseIO func (Array sa) =
  if isTrue# (len ==# 0#)
  then return empty
  else
    case indexSmallArray# sa 0# of
      (# a #) ->
        do  b <- func a
            (SMA sma) <- newIO len b
            loop sma 1#
  where
    !len = sizeofSmallArray# sa

    loop sma i =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa i of
          (# a #) ->
            do  b <- func a
                writeIO sma i b
                loop sma (i +# 1#)
      else
        freezeIO sma



-- TRAVERSE M


traverseM :: (Monad m) => (forall r. IO r -> m r) -> (a -> m b) -> Array a -> m (Array b)
traverseM io func (Array sa) =
  if isTrue# (len ==# 0#)
  then return empty
  else
    case indexSmallArray# sa 0# of
      (# a #) ->
        do  b <- func a
            (SMA sma) <- io (newIO len b)
            loop sma 1#
  where
    !len = sizeofSmallArray# sa

    loop sma i =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa i of
          (# a #) ->
            do  b <- func a
                io (writeIO sma i b)
                loop sma (i +# 1#)
      else
        io (freezeIO sma)



-- TRAVERSE MAYBE


traverseMaybe :: (a -> Maybe b) -> Array a -> Maybe (Array b)
traverseMaybe func (Array sa) =
  if isTrue# (len ==# 0#)
  then Just empty
  else
    case indexSmallArray# sa 0# of
      (# a #) ->
        do  b <- func a
            runST $ ST $ \s0 ->
              case newSmallArray# len b s0 of
                (# s1, sma #) ->
                  traverseMaybe# sa sma func len s1
  where
    !len = sizeofSmallArray# sa


{-# INLINE traverseMaybe# #-}
traverseMaybe# :: SmallArray# a -> SmallMutableArray# s b -> (a -> Maybe b) -> Int# -> State# s -> (# State# s, Maybe (Array b) #)
traverseMaybe# sa sma func len s =
    loop 1# s
  where
    loop i s0 =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa i of
          (# a #) ->
            case func a of
              Just b  -> case writeSmallArray# sma i b s0 of { s1 -> loop (i +# 1#) s1 }
              Nothing -> (# s0, Nothing #)
      else
        case unsafeFreezeSmallArray# sma s0 of { (# s1, sa' #) -> (# s1, Just (Array sa') #) }



-- TRAVERSE EITHER


traverseEither :: (a -> Either x b) -> Array a -> Either x (Array b)
traverseEither func (Array sa) =
  if isTrue# (len ==# 0#)
  then Right empty
  else
    case indexSmallArray# sa 0# of
      (# a #) ->
        case func a of
          Left  x -> Left x
          Right b ->
            runST $ ST $ \s0 ->
              case newSmallArray# len b s0 of
                (# s1, sma #) ->
                  traverseEither# sa sma func len s1
  where
    !len = sizeofSmallArray# sa


{-# INLINE traverseEither# #-}
traverseEither# :: SmallArray# a -> SmallMutableArray# s b -> (a -> Either x b) -> Int# -> State# s -> (# State# s, Either x (Array b) #)
traverseEither# sa sma func len s =
    loop 1# s
  where
    loop i s0 =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa i of
          (# a #) ->
            case func a of
              Right b -> case writeSmallArray# sma i b s0 of { s1 -> loop (i +# 1#) s1 }
              Left  x -> (# s0, Left x #)
      else
        case unsafeFreezeSmallArray# sma s0 of { (# s1, sa' #) -> (# s1, Right (Array sa') #) }



-- FOLD


foldl :: (a -> b -> b) -> b -> Array a -> b
foldl step base (Array sa) =
    loop 0# base
  where
    !len = sizeofSmallArray# sa

    loop i state =
      if isTrue# (i <# len)
      then case indexSmallArray# sa i of { (# a #) -> loop (i +# 1#) (step a state) }
      else state


foldr :: (a -> b -> b) -> b -> Array a -> b
foldr step base (Array sa) =
    loop (sizeofSmallArray# sa -# 1#) base
  where
    loop i state =
      if isTrue# (i >=# 0#)
      then case indexSmallArray# sa i of { (# a #) -> loop (i -# 1#) (step a state) }
      else state


foldlM :: (Monad m) => (a -> s -> m s) -> s -> Array a -> m s
foldlM step base (Array sa) =
    loop 0# base
  where
    !len = sizeofSmallArray# sa

    loop i state =
      if isTrue# (i <# len)
      then case indexSmallArray# sa i of { (# a #) -> loop (i +# 1#) =<< step a state }
      else return state



-- FOLDL MAP IO


foldlMapIO :: (a -> s -> IO (b, s)) -> s -> Array a -> IO (Array b, s)
foldlMapIO step base (Array sa) =
  let
    !len = sizeofSmallArray# sa
  in
  if isTrue# (len ==# 0#)
  then return (empty, base)
  else
    case indexSmallArray# sa 0# of
      (# a #) ->
        do  (b,s) <- step a base
            sma <- newIO len b
            foldlMapIO# step s sa sma len


foldlMapIO# :: (a -> s -> IO (b, s)) -> s -> SmallArray# a -> SMA b -> Int# -> IO (Array b, s)
foldlMapIO# step base sa (SMA sma) len =
    loop 1# base
  where
    loop i state =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa i of
          (# a #) ->
            do  (b, s) <- step a state
                writeIO sma i b
                loop (i +# 1#) s
      else
        do  array <- freezeIO sma
            return (array, state)



-- TO LIST


toList :: Array a -> [a]
toList (Array sa) =
    loop 0#
  where
    !len = sizeofSmallArray# sa

    loop i =
      if isTrue# (i <# len)
      then case indexSmallArray# sa i of { (# a #) -> a : loop (i +# 1#) }
      else []



-- TO LIST WITH


toListWith :: (a -> b) -> Array a -> [b]
toListWith func (Array sa) =
    loop 0#
  where
    !len = sizeofSmallArray# sa

    loop i =
      if isTrue# (i <# len)
      then case indexSmallArray# sa i of { (# a #) -> func a : loop (i +# 1#) }
      else []



-- ADD TO LIST


addToList :: Array a -> [a] -> [a]
addToList (Array sa) list =
    loop 0#
  where
    !len = sizeofSmallArray# sa

    loop i =
      if isTrue# (i <# len)
      then case indexSmallArray# sa i of { (# a #) -> a : loop (i +# 1#) }
      else list



-- FROM LIST


fromList :: [a] -> Array a
fromList list =
  case list of
    []   -> empty
    x:xs -> fromList1 x xs


fromList1 :: a -> [a] -> Array a
fromList1 x xs =
  runST $ ST $ \s0 ->
    let
      !(I# len) = 1 + List.length xs
    in
    case newSmallArray# len x        s0 of { (# s1, sma #) ->
    case fromList1# sma 1# xs        s1 of {    s2         ->
    case unsafeFreezeSmallArray# sma s2 of { (# s3, sa  #) -> (# s3, Array sa #) }}}


fromList1# :: SmallMutableArray# s a -> Int# -> [a] -> State# s -> State# s
fromList1# sma =
    loop
  where
    loop i list s0 =
      case list of
        []   -> s0
        x:xs -> case writeSmallArray# sma i x s0 of { s1 -> loop (i +# 1#) xs s1 }



-- FROM LIST WITH


fromListWith :: (a -> b) -> [a] -> Array b
fromListWith func list =
  case list of
    []   -> empty
    x:xs -> fromListWith1 func x xs


fromListWith1 :: (a -> b) -> a -> [a] -> Array b
fromListWith1 func x xs =
  runST $ ST $ \s0 ->
    let
      !(I# len) = 1 + List.length xs
    in
    case newSmallArray# len (func x)   s0 of { (# s1, sma #) ->
    case fromListWith1# func sma 1# xs s1 of {    s2         ->
    case unsafeFreezeSmallArray# sma   s2 of { (# s3, sa  #) -> (# s3, Array sa #) }}}


fromListWith1# :: (a -> b) -> SmallMutableArray# s b -> Int# -> [a] -> State# s -> State# s
fromListWith1# func sma =
    loop
  where
    loop i list s0 =
      case list of
        []   -> s0
        x:xs -> case writeSmallArray# sma i (func x) s0 of { s1 -> loop (i +# 1#) xs s1 }



-- IO HELPERS


data SMA a =
  SMA (SmallMutableArray# RealWorld a)


newIO :: Int# -> a -> IO (SMA a)
newIO len a =
  IO $ \s0 -> case newSmallArray# len a s0 of { (# s1, sma #) -> (# s1, SMA sma #) }


writeIO :: SmallMutableArray# RealWorld a -> Int# -> a -> IO ()
writeIO sma i a =
  IO $ \s0 -> case writeSmallArray# sma i a s0 of { s1 -> (# s1, () #) }


freezeIO :: SmallMutableArray# RealWorld a -> IO (Array a)
freezeIO sma =
  IO $ \s0 -> case unsafeFreezeSmallArray# sma s0 of { (# s1, sa #) -> (# s1, Array sa #) }



-- EQ INSTANCE


instance Eq a => Eq (Array a) where
  (==) = equalHelp (==)

instance Eq1 Array where
  liftEq = equalHelp


equalHelp :: (a -> b -> Bool) -> Array a -> Array b -> Bool
equalHelp eq (Array sa1) (Array sa2) =
    isTrue# (len1 ==# len2) && loop 0#
  where
    !len1 = sizeofSmallArray# sa1
    !len2 = sizeofSmallArray# sa2

    loop i =
      if isTrue# (i <# len1)
      then
        case indexSmallArray# sa1 i of { (# x #) ->
        case indexSmallArray# sa2 i of { (# y #) -> eq x y && loop (i +# 1#) }}
      else
        True



-- ORD INSTANCE


instance Ord a => Ord (Array a) where
  compare (Array sa1) (Array sa2) =
    let
      !len1 = sizeofSmallArray# sa1
      !len2 = sizeofSmallArray# sa2
    in
    if isTrue# (len1 <# len2)
    then compareHelp sa1 sa2 len1 LT
    else compareHelp sa1 sa2 len2 (if isTrue# (len1 ==# len2) then EQ else GT)


compareHelp :: (Ord a) => SmallArray# a -> SmallArray# a -> Int# -> Ordering -> Ordering
compareHelp sa1 sa2 len finalAnswer =
    loop 0#
  where
    loop i =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa1 i of
          (# a1 #) ->
            case indexSmallArray# sa2 i of
              (# a2 #) ->
                case compare a1 a2 of
                  EQ -> loop (i +# 1#)
                  ne -> ne
      else
        finalAnswer

