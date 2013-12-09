module Lazy.Stream ( head, tail
                   , cons, cons', iterate, repeat, cycle
                   , map, apply, zip, zipWith, scanl
                   , take, drop, splitAt
                   , sampleOn
                   ) where

{-| Library for Infinite Streams 

# Observe
@docs head, tail, sampleOn, take, drop, splitAt

# Create
@docs cons', iterate, repeat, cycle

# Transform
@docs map, apply, zip, zipWith, scanl

-}

import Basics ((.), (<|), (-), id, fst, snd)
import open Lazy
import Signal ((<~), foldp, Signal)

data Stream a = S (Lazy (a, Stream a))

unS : Stream a -> (a, Stream a)
unS (S t) = force t

head : Stream a -> a
head = fst . unS

tail : Stream a -> Stream a
tail = snd . unS

cons : a -> (() -> Stream a) -> Stream a
cons x txs = let mtxs = lazy txs in
  S . lazy <| \() ->
  (x, force mtxs)

cons' : (() -> (a, Stream a)) -> Stream a
cons' = S . lazy

iterate : (a -> a) -> a -> Stream a
iterate f x = cons' <| \() ->
  (x, map f (iterate f x))

repeat : a -> Stream a
repeat x = iterate id x
  
cycle : a -> [a] -> Stream a
cycle x xs = let cycle' ys = case ys of
                   [] -> cons' <| \() ->
                     (x, cycle' xs)
             in cycle' []

map : (a -> b) -> Stream a -> Stream b
map f xs = cons' <| \() ->
  (f (head xs), map f (tail xs))

apply : Stream (a -> b) -> Stream a -> Stream b
apply fs xs = zipWith (<|) fs xs
  
zip : Stream a -> Stream b -> Stream (a, b)
zip = zipWith (\x y -> (x,y))

zipWith : (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f xs ys = cons' <| \() ->
  (f (head xs) (head ys),
   zipWith f (tail xs) (tail ys))

scanl : (a -> b -> b) -> b -> Stream a -> Stream b
scanl f init xs = cons' <| \() ->
  (init,
   scanl f (f (head xs) init) (tail xs))

take : Int -> Stream a -> [a]
take n xs = fst <| splitAt n xs

drop : Int -> Stream a -> Stream a
drop n xs = snd <| splitAt n xs

splitAt : Int -> Stream a -> ([a], Stream a)
splitAt n xs = case n of
  0 -> ([], xs)
  n -> let (heads, end) = splitAt (n - 1) (tail xs)
       in (head xs :: heads, end)

sampleOn : Stream a -> Signal b -> Signal a
sampleOn str sig = let tails = foldp (\_ -> tail) str sig in
                   head <~ tails
