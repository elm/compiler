module Data.Index
  ( ZeroBased
  , first
  , second
  , third
  , next
  , toMachine
  , toHuman
  , indexedMap
  , indexedTraverse
  , indexedForA
  , VerifiedList(..)
  , indexedZipWith
  , VerifiedList2(..)
  , indexedZipWith2
  --
  , dZeroBased
  , eZeroBased
  )
  where


import qualified Bytes.Decode as D
import qualified Bytes.Encode as E



-- ZERO BASED


newtype ZeroBased = ZeroBased Int
  deriving (Eq, Ord)


first :: ZeroBased
first =
  ZeroBased 0


second :: ZeroBased
second =
  ZeroBased 1


third :: ZeroBased
third =
  ZeroBased 2


{-# INLINE next #-}
next :: ZeroBased -> ZeroBased
next (ZeroBased i) =
  ZeroBased (i + 1)



-- DESTRUCT


toMachine :: ZeroBased -> Int
toMachine (ZeroBased index) =
  index


toHuman :: ZeroBased -> Int
toHuman (ZeroBased index) =
  index + 1



-- INDEXED MAP


{-# INLINE indexedMap #-}
indexedMap :: (ZeroBased -> a -> b) -> [a] -> [b]
indexedMap func xs =
  zipWith func (map ZeroBased [0 .. length xs]) xs


{-# INLINE indexedTraverse #-}
indexedTraverse :: (Applicative f) => (ZeroBased -> a -> f b) -> [a] -> f [b]
indexedTraverse func xs =
  sequenceA (indexedMap func xs)


{-# INLINE indexedForA #-}
indexedForA :: (Applicative f) => [a] -> (ZeroBased -> a -> f b) -> f [b]
indexedForA xs func =
  sequenceA (indexedMap func xs)



-- INDEXED ZIP


data VerifiedList a
  = LengthMatch [a]
  | LengthMismatch Int Int


indexedZipWith :: (ZeroBased -> a -> b -> c) -> [a] -> [b] -> VerifiedList c
indexedZipWith func =
    go 0 []
  where
    f = func . ZeroBased

    go i revs listX listY =
      case (listX, listY) of
        ([]  , []  ) -> LengthMatch (reverse revs)
        (x:xs, y:ys) -> go (i + 1) (f i x y : revs) xs ys
        (_   , _   ) -> LengthMismatch (i + length listX) (i + length listY)



-- INDEXED ZIP 2


data VerifiedList2 a
  = LengthMatch2 a a [a]
  | LengthMismatch2 Int Int


indexedZipWith2 :: (ZeroBased -> a -> b -> c) -> a -> a -> [a] -> [b] -> VerifiedList2 c
indexedZipWith2 func a b cs list =
  case list of
    []     -> LengthMismatch2 (2 + length cs) 0
    [_]    -> LengthMismatch2 (2 + length cs) 1
    x:y:zs -> go 2 cs zs (f 0 a x) (f 1 b y) []
  where
    f = func . ZeroBased

    go i listX listY v1 v2 revs =
      case (listX, listY) of
        ([]  , []  ) -> LengthMatch2 v1 v2 (reverse revs)
        (x:xs, y:ys) -> go (i + 1) xs ys v1 v2 (f i x y : revs)
        (_   , _   ) -> LengthMismatch2 (i + length listX) (i + length listY)



-- BINARY


dZeroBased :: D.Decoder ZeroBased
dZeroBased =
  ZeroBased <$> D.int


eZeroBased :: ZeroBased -> E.Builder
eZeroBased (ZeroBased n) =
  E.int n
