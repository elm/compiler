{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
module Type.Pools
  ( Pools
  , init
  , read
  , write
  , modify
  , growIfNeeded
  --
  , RankTable
  , rankTable
  , indexedTraverse_
  , traverseRanks_
  )
  where


import Prelude hiding (init, read)
import Foreign.Storable (sizeOf)
import GHC.Exts (isTrue#)
import GHC.Prim
import GHC.Int (Int(..))
import GHC.IO (IO(IO), unIO)

import Type.Variable (Variable, Rank(..))



--------------------------------------------------------------------------------
-- POOLS -----------------------------------------------------------------------
--------------------------------------------------------------------------------



-- POOLS


data Pools =
  Pools (MutableArray# RealWorld [Variable])



-- INIT


init :: Int -> IO Pools
init (I# len) =
  IO $ \s0 ->
    case newArray# len [] s0 of
      (# s1, ma #) -> (# s1, Pools ma #)



-- READ


read :: Rank -> Pools -> IO [Variable]
read (Rank (I# i)) (Pools ma) =
  IO $ \s0 -> readArray# ma i s0



-- WRITE


write :: Rank -> [Variable] -> Pools -> IO ()
write (Rank (I# i)) vars (Pools ma) =
  IO $ \s0 ->
    case writeArray# ma i vars s0 of
      s1 -> (# s1, () #)



-- MODIFY


{-# INLINE modify #-}
modify :: Rank -> ([Variable] -> [Variable]) -> Pools -> IO ()
modify (Rank (I# i)) func (Pools ma) =
  IO $ \s0 ->
    case readArray#  ma i             s0 of { (# s1, vars #) ->
    case writeArray# ma i (func vars) s1 of {    s2          -> (# s2, () #) }}



-- GROW IF NEEDED


growIfNeeded :: Rank -> Pools -> IO Pools
growIfNeeded (Rank (I# i)) pools@(Pools ma) =
  if isTrue# (i <# len)
  then return pools
  else
    IO $ \s0 ->
      case newArray# (nearestSquare i) []     s0 of { (# s1, ma' #) ->
      case copyMutableArray# ma 0# ma' 0# len s1 of {    s2         ->
        (# s2, (Pools ma') #)
      }}
  where
    !len = sizeofMutableArray# ma


nearestSquare :: Int# -> Int#
nearestSquare i =
    size -# word2Int# (clz# (int2Word# i))
  where
    !(I# size) = 8 * sizeOf (0 :: Word)



--------------------------------------------------------------------------------
-- RANK TABLE ------------------------------------------------------------------
--------------------------------------------------------------------------------



-- RANK TABLE


data RankTable =
  RankTable (Array# [Variable])


rankTable :: Rank -> [Variable] -> (Variable -> IO Rank) -> IO RankTable
rankTable (Rank (I# maxRank)) vars0 setup =
  IO $ \s0 ->
    case newArray# (maxRank +# 1#) [] s0 of { (# s1, ma #) ->
    case unIO (mapM_ (add ma) vars0)  s1 of { (# s2, () #) ->
    case unsafeFreezeArray# ma        s2 of { (# s3, rt #) ->
      (# s3, RankTable rt #)
    }}}
  where
    add ma var =
      do  !(Rank (I# i)) <- setup var
          IO $ \s0 ->
            case readArray#  ma i            s0 of { (# s1, vars #) ->
            case writeArray# ma i (var:vars) s1 of {    s2          -> (# s2, () #) }}



-- INDEXED TRAVERSE


indexedTraverse_ :: (Rank -> [Variable] -> IO ()) -> RankTable -> IO ()
indexedTraverse_ adjust (RankTable rt) =
    loop 0#
  where
    !len = sizeofArray# rt

    loop i =
      if isTrue# (i <# len)
      then
        case indexArray# rt i of
          (# vars #) ->
            adjust (Rank (I# i)) vars >> loop (i +# 1#)
      else
        return ()



-- TRAVERSE RANKS


traverseRanks_ :: (Variable -> IO ()) -> (Variable -> IO ()) -> RankTable -> IO ()
traverseRanks_ goNormal goLast (RankTable rt) =
    loop 0#
  where
    !boundary = sizeofArray# rt -# 1#

    loop i =
      if isTrue# (i <# boundary)
      then case indexArray# rt i of { (# vars #) -> mapM_ goNormal vars >> loop (i +# 1#) }
      else case indexArray# rt i of { (# vars #) -> mapM_ goLast vars }

