{-# LANGUAGE BangPatterns, MagicHash, TemplateHaskell, UnboxedTuples #-}
module AST.Prim.TypeIndex
  ( Index(..)
  , get
  , lookup
  , imap
  --
  , Registry
  , empty
  , fromArray
  , fromDict
  --
  , toArray
  --
  , register
  , registerNext
  --
  , eIndex
  , dIndex
  )
  where


import Prelude hiding (lookup)
import Data.Map ((!))
import qualified Data.Map as Map
import GHC.Exts (isTrue#)
import GHC.Int (Int(..))
import GHC.Prim
import GHC.ST (ST(ST), runST)

import qualified Array as A
import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Crash



-- INDEX


newtype Index = Index Int
  deriving (Eq, Ord)


get :: Index -> A.Array a -> a
get (Index i) array =
  A.get i array


lookup :: (Ord k) => k -> Registry k -> Index
lookup k (Registry dict) =
  dict ! k



-- IMAP


imap :: (Index -> b) -> A.Array a -> A.Array b
imap func (A.Array sa) =
  let
    !len = sizeofSmallArray# sa
  in
  if isTrue# (len ==# 0#)
  then A.empty
  else
    runST $ ST $ \s0 ->
      case newSmallArray# len (func (Index 0)) s0 of
        (# s1, sma #) ->
          imap# func sma len s1


imap# :: (Index -> b) -> SmallMutableArray# s b -> Int# -> State# s -> (# State# s, A.Array b #)
imap# func sma len s =
    go 1# s
  where
    go i s0 =
      if isTrue# (i <# len)
      then
        case writeSmallArray# sma i (func (Index (I# i))) s0 of
          s1 ->
            go (i +# 1#) s1
      else
        case unsafeFreezeSmallArray# sma s0 of
          (# s1, sa #) ->
            (# s1, A.Array sa #)



-- REGISTRY


newtype Registry k =
  Registry (Map.Map k Index)


empty :: Registry k
empty =
  Registry Map.empty


fromArray :: (Ord a) => A.Array a -> Registry a
fromArray (A.Array sa) =
    go 0# []
  where
    !len = sizeofSmallArray# sa

    go i pairs =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa i of
          (# a #) ->
            go (i +# 1#) $! (a, Index (I# i)) : pairs
      else
        Registry $! Map.fromList pairs


fromDict :: (Ord k) => Map.Map k v -> Registry k
fromDict dict =
  Registry $ snd $ Map.mapAccum (\i _ -> (i + 1, Index i)) 0 dict



-- TO ARRAY


toArray :: Registry a -> A.Array a
toArray (Registry dict) =
  runST $ ST $ \s0 ->
    let
      !(I# len) = Map.size dict
    in
    case newSmallArray# len blank                        s0 of { (# s1, sma #) ->
    case toArray# sma (Map.foldrWithKey cons Empty dict) s1 of {    s2         ->
    case unsafeFreezeSmallArray# sma                     s2 of { (# s3, sa  #) ->
      (# s3, A.Array sa #)
    }}}


data Pairs k
  = Cons k Int# (Pairs k)
  | Empty


cons :: k -> Index -> Pairs k -> Pairs k
cons k (Index (I# i)) pairs =
  Cons k i pairs


toArray# :: SmallMutableArray# s k -> Pairs k -> State# s -> State# s
toArray# sma pairs s0 =
  case pairs of
    Empty ->
      s0

    Cons key i rest ->
      case writeSmallArray# sma i key s0 of
        s1 ->
          toArray# sma rest s1


{-# NOINLINE blank #-}
blank :: a
blank =
  $(Crash.crash 'blank) "missing type variable"



-- REGISTER


register :: (Ord k) => k -> Registry k -> (Index, Registry k)
register key registry@(Registry dict) =
  case Map.lookup key dict of
    Nothing ->
      let
        !index = Index (Map.size dict)
      in
      (index, Registry (Map.insert key index dict))

    Just index ->
      (index, registry)


registerNext :: (Ord k) => (Int -> k) -> Int -> Registry k -> (Int, k, Index, Registry k)
registerNext toKey i registry@(Registry dict) =
  let
    key = toKey i
  in
  if Map.member key dict
  then registerNext toKey (i + 1) registry
  else
    let
      !index = Index (Map.size dict)
    in
    ( i + 1, key, index, Registry (Map.insert key index dict) )



-- ENCODE/DECODE


eIndex :: Index -> E.Builder
eIndex (Index i) =
  E.u8 (fromIntegral i)


dIndex :: D.Decoder Index
dIndex =
  Index . fromIntegral <$> D.u8

