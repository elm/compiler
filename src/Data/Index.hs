module Data.Index
  ( ZeroBased
  , first
  , unsafe
  , next
  , toZeroBased
  , toOneBased
  , indexedMap
  , indexedTraverse
  , indexedForA
  , VerifiedList(..)
  , indexedZipWith
  , indexedZipWithA
  )
  where


import Control.Monad (liftM)
import Data.Binary



-- ZERO BASED


newtype ZeroBased = ZeroBased Int
  deriving (Eq, Ord)


first :: ZeroBased
first =
  ZeroBased 0


unsafe :: Int -> ZeroBased
unsafe n =
  if n < 0 then
    ZeroBased n
  else
    error "Compiler error, unacceptable call of Index.unsafe"


{-# INLINE next #-}
next :: ZeroBased -> ZeroBased
next (ZeroBased i) =
  ZeroBased (i + 1)



-- DESTRUCT


toZeroBased :: ZeroBased -> Int
toZeroBased (ZeroBased index) =
  index


toOneBased :: ZeroBased -> Int
toOneBased (ZeroBased index) =
  index + 1



-- INDEXED MAP


{-# INLINE indexedMap #-}
indexedMap :: (ZeroBased -> a -> b) -> [a] -> [b]
indexedMap func list =
  zipWith func (map ZeroBased [0 .. length list]) list


{-# INLINE indexedTraverse #-}
indexedTraverse :: (Applicative f) => (ZeroBased -> a -> f b) -> [a] -> f [b]
indexedTraverse func list =
  sequenceA (indexedMap func list)


{-# INLINE indexedForA #-}
indexedForA :: (Applicative f) => [a] -> (ZeroBased -> a -> f b) -> f [b]
indexedForA list func =
  sequenceA (indexedMap func list)



-- VERIFIED/INDEXED ZIP


data VerifiedList a
  = LengthMatch [a]
  | LengthMismatch Int Int


indexedZipWith :: (ZeroBased -> a -> b -> c) -> [a] -> [b] -> VerifiedList c
indexedZipWith func listX listY =
  indexedZipWithHelp func 0 listX listY []


indexedZipWithHelp :: (ZeroBased -> a -> b -> c) -> Int -> [a] -> [b] -> [c] -> VerifiedList c
indexedZipWithHelp func index listX listY revListZ =
  case (listX, listY) of
    ([], []) ->
      LengthMatch (reverse revListZ)

    (x:xs, y:ys) ->
      indexedZipWithHelp func (index + 1) xs ys $
        func (ZeroBased index) x y : revListZ

    (_, _) ->
      LengthMismatch (index + length listX) (index + length listY)


indexedZipWithA :: (Applicative f) => (ZeroBased -> a -> b -> f c) -> [a] -> [b] -> f (VerifiedList c)
indexedZipWithA func listX listY =
  case indexedZipWith func listX listY of
    LengthMatch list ->
      LengthMatch <$> sequenceA list

    LengthMismatch x y ->
      pure (LengthMismatch x y)



-- BINARY


instance Binary ZeroBased where
  get = liftM ZeroBased get
  put (ZeroBased n) = put n

