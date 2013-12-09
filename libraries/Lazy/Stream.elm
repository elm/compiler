module Lazy.Stream ( hd, tl
                   , mkStream , iterate, repeat, cycle
                   , map, apply, zip, zipWith, scanl
                   , take, drop, splitAt
                   , observeWhen
                   ) where

{-| Library for Infinite Streams 

# Observe
@docs hd, tl, observeWhen, take, drop, splitAt

# Create
@docs mkStream, iterate, repeat, cycle

# Transform
@docs map, apply, zip, zipWith, scanl

-}

import Basics ((.), (<|), (-), id, fst, snd)
import open Lazy
import Signal ((<~), foldp, Signal)

data Stream a = S (Lazy { hd : a, tl : Stream a })

hd : Stream a -> a
hd (S t) = (run t).hd

tl : Stream a -> Stream a
tl (S t) = (run t).tl

mkStream : (() -> { hd : a, tl : Stream a }) -> Stream a
mkStream = S . thunk

iterate : (a -> a) -> a -> Stream a
iterate f x = mkStream <| \() ->
  { hd = x
  , tl = map f (iterate f x)
  }

repeat : a -> Stream a
repeat x = iterate id x
  
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

scanl : (a -> b -> b) -> b -> Stream a -> Stream b
scanl f init xs = mkStream <| \() ->
  { hd = init
  , tl = scanl f (f (hd xs) init) (tl xs)
  }

take : Int -> Stream a -> [a]
take n xs = fst <| splitAt n xs

drop : Int -> Stream a -> Stream a
drop n xs = snd <| splitAt n xs

splitAt : Int -> Stream a -> ([a], Stream a)
splitAt n xs = case n of
  0 -> ([], xs)
  n -> let (hds, end) = splitAt (n - 1) (tl xs)
       in (hd xs :: hds, end)

observeWhen : Stream a -> Signal b -> Signal a
observeWhen str sig = let tls = foldp (\_ -> tl) str sig in
                      hd <~ tls
