{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Rank2Types #-}
module Reporting.Result
  ( Result(..)
  , run
  , ok
  , warn
  , throw
  , mapError
  )
  where


import qualified Data.OneOrMore as OneOrMore
import qualified Reporting.Warning as Warning



-- RESULT


newtype Result info warnings error a =
  Result (
    forall result.
      info
      -> warnings
      -> (info -> warnings -> OneOrMore.OneOrMore error -> result)
      -> (info -> warnings -> a -> result)
      -> result
  )


run :: Result () [w] e a -> ([w], Either [e] a)
run (Result k) =
  k () []
    (\() w e -> (reverse w, Left (OneOrMore.toList e)))
    (\() w a -> (reverse w, Right a))



-- HELPERS


ok :: a -> Result i w e a
ok a =
  Result $ \i w _ good ->
    good i w a


warn :: Warning.Warning -> Result i [Warning.Warning] e ()
warn warning =
  Result $ \i warnings _ good ->
    good i (warning:warnings) ()


throw :: e -> Result i w e a
throw e =
  Result $ \i w bad _ ->
    bad i w (OneOrMore.one e)


mapError :: (e -> e') -> Result i w e a -> Result i w e' a
mapError func (Result k) =
  Result $ \i w bad good ->
    let
      bad1 i1 w1 e1 =
        bad i1 w1 (OneOrMore.map func e1)
    in
    k i w bad1 good



-- FANCY INSTANCE STUFF


instance Functor (Result i w e) where
  fmap func (Result k) =
    Result $ \i w bad good ->
      let
        good1 i1 w1 value =
          good i1 w1 (func value)
      in
      k i w bad good1


instance Applicative (Result i w e) where
  pure = ok

  (<*>) (Result kf) (Result kv) =
    Result $ \i w bad good ->
      let
        bad1 i1 w1 e1 =
          let
            bad2 i2 w2 e2 = bad i2 w2 (OneOrMore.more e1 e2)
            good2 i2 w2 _value = bad i2 w2 e1
          in
          kv i1 w1 bad2 good2

        good1 i1 w1 func =
          let
            bad2 i2 w2 e2 = bad i2 w2 e2
            good2 i2 w2 value = good i2 w2 (func value)
          in
          kv i1 w1 bad2 good2
      in
      kf i w bad1 good1


instance Monad (Result i w e) where
  return = ok

  (>>=) (Result ka) callback =
    Result $ \i w bad good ->
      let
        good1 i1 w1 a =
          case callback a of
            Result kb -> kb i1 w1 bad good
      in
      ka i w bad good1
