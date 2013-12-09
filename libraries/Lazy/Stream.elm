module Lazy.Stream ( hd, tl, mkStream, repeat, cycle
                   , map, apply, zip, zipWith, iterate
                   , take, observeWhen
                   ) where

{-| Library for Infinite Streams -}

import Basics ((.), (<|), (-))
import open Lazy
import Signal ((<~), foldp, Signal)

data Stream a = S (Lazy { hd : a, tl : Stream a })

hd : Stream a -> a
hd (S t) = (run t).hd

tl : Stream a -> Stream a
tl (S t) = (run t).tl

mkStream : (() -> { hd : a, tl : Stream a }) -> Stream a
mkStream = S . thunk

repeat : a -> Stream a
repeat x = mkStream <| \() ->
  { hd = x, tl = repeat x }
  
cycle : a -> [a] -> Stream a
cycle x xs = let cycle' ys = case ys of
                   [] -> mkStream <| \() ->
                     { hd = x
                     , tl = cycle' xs }
                   (y :: ys) -> mkStream <| \() ->
                     { hd = y
                     , tl = cycle' ys }
             in cycle' []

map : (a -> b) -> Stream a -> Stream b
map f xs = mkStream <| \() ->
  { hd = f (hd xs)
  , tl = map f (tl xs)
  }

apply : Stream (a -> b) -> Stream a -> Stream b
apply fs xs = zipWith (<|) fs xs
  
zip : Stream a -> Stream b -> Stream (a, b)
zip = zipWith (\x y -> (x,y))

zipWith : (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f xs ys = mkStream <| \() ->
  { hd = f (hd xs) (hd ys)
  , tl = zipWith f (tl xs) (tl ys)
  }

iterate : (a -> a) -> a -> Stream a
iterate f x = mkStream <| \() ->
  { hd = x
  , tl = map f (iterate f x)
  }

take : Int -> Stream a -> [a]
take n xs = case n of
  0 -> []
  n -> hd xs :: take (n - 1) (tl xs)

observeWhen : Stream a -> Signal b -> Signal a
observeWhen str sig = let tls = foldp (\_ -> tl) str sig in
                      hd <~ tls
