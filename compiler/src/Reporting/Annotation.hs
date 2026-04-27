{-# LANGUAGE ExtendedLiterals, MagicHash #-}
module Reporting.Annotation
  ( Located(..)
  , Position(..)
  , Region(..)
  , region
  , traverse
  , toValue
  , merge
  , at
  , toRowCol
  , toRow
  , toCol
  , toRegion
  , mergeRegions
  , zero
  )
  where


import Prelude hiding (traverse)
import Data.Binary (Binary, get, put)
import GHC.Exts (isTrue#)
import GHC.Prim
import GHC.Word (Word(..), Word64(..))



-- LOCATED


data Located a =
  At Region a  -- PERF see if unpacking region is helpful
  deriving (Eq, Ord)


instance Functor Located where
  fmap f (At r a) =
    At r (f a)


traverse :: (Functor f) => (a -> f b) -> Located a -> f (Located b)
traverse func (At r value) =
  At r <$> func value


toValue :: Located a -> a
toValue (At _ value) =
  value


merge :: Located a -> Located b -> value -> Located value
merge (At r1 _) (At r2 _) value =
  At (mergeRegions r1 r2) value



-- POSITION
--
-- GHC represents Word8 through Word64 as a 64-bit value, so here we get a bit
-- of extra memory density by packing two Word32 into one Word64.
--
-- It is stored as (y,x) so (pos + 1) increments x, and (pos + 0x100000000)
-- increments y. The initial value is (0,0) so conversion is needed to get
-- "editor coordinates" which start at (1,1)


data Position = Position Word64#


at :: Position -> Position -> a -> Located a
at (Position start) (Position end) a =
  At (Region start end) a


{-# INLINE toRowCol #-}
toRowCol :: Word64# -> (Int, Int)
toRowCol pos =
  ( fromIntegral (toRow pos)
  , fromIntegral (toCol pos)
  )


{-# INLINE toRow #-}
toRow :: Word64# -> Word64
toRow pos =
  W64# (uncheckedShiftRL64# pos 32#)


{-# INLINE toCol #-}
toCol :: Word64# -> Word
toCol pos =
  W# (narrow32Word# (word64ToWord# pos))


instance Eq Position where
  (==) (Position x) (Position y) = isTrue# (eqWord64# x y)

instance Ord Position where
  compare (Position x) (Position y)
    | isTrue# (ltWord64# x y) = LT
    | isTrue# (gtWord64# x y) = GT
    | otherwise               = EQ




-- REGION


data Region =
  Region Word64# Word64#


{-# INLINE region #-}
region :: Position -> Position -> Region
region (Position s) (Position e) =
  Region s e


toRegion :: Located a -> Region
toRegion (At r _) =
  r


mergeRegions :: Region -> Region -> Region
mergeRegions (Region start _) (Region _ end) =
  Region start end


zero :: Region
zero =
  Region 0#Word64 0#Word64


instance Eq Region where
  (==) (Region s e) (Region s' e') =
    isTrue# (eqWord64# s s') &&
    isTrue# (eqWord64# e e')


instance Ord Region where
  compare (Region s e) (Region s' e')
    | isTrue# (ltWord64# s s') = LT
    | isTrue# (gtWord64# s s') = GT
    | isTrue# (ltWord64# e e') = LT
    | isTrue# (gtWord64# e e') = GT
    | otherwise                = EQ


instance Binary Region where
  get =
    do  (W64# s) <- get
        (W64# e) <- get
        pure $ Region s e

  put (Region s e) =
    put (W64# s) >> put (W64# e)
