module Lazy.Stream ( head, tail
                   , cons, cons', iterate, repeat, cycle
                   , map, apply, zip, zipWith, scanl
                   , take, drop, splitAt
                   , sampleOn
                   , filter, takeWhile, dropWhile, splitWith
                   ) where

{-| This library is for creating and manipulating infinite streams 

# Create
@docs cons, cons', repeat, cycle, iterate

# Use
@docs head, tail, sampleOn, take, takeWhile, drop, dropWhile, splitAt, splitWith

# Transform
@docs map, zip, zipWith, scanl, filter, apply

-}

import Basics ((.), (<|), (-), id, fst, snd)
import open Lazy
import Signal ((<~), foldp, Signal)

data Stream a = S (Lazy (a, Stream a))

unS : Stream a -> (a, Stream a)
unS (S t) = force t

{-| Get the first element of a stream, called the `head`.

```haskell
head ones == 1
```
-}
head : Stream a -> a
head = fst . unS

{-| Drop the `head` of a stream, leaving you with the `tail`.

```haskell
-- 1, 1, 1, 1, ...
stillAllOnes = tail 
```
-}
tail : Stream a -> Stream a
tail = snd . unS

{-| Create a stream:

```haskell
-- 1, 1, 1, 1, ...
ones = cons 1 (\() -> ones)
```
 -}
cons : a -> (() -> Stream a) -> Stream a
cons x txs = let mtxs = lazy txs in
  S . lazy <| \() ->
  (x, force mtxs)

{-| Create a stream that is slightly more lazy. Notice that the
head of the stream is defined within the thunk, so its evaluation
is delayed. This is nice when each element of the stream is costly to compute.

```haskell
-- 1, 1, 1, 1, ...
ones = cons' (\_ -> (1,ones))
```
-}
cons' : (() -> (a, Stream a)) -> Stream a
cons' = S . lazy

{-| Iteratively apply a function to a value:

```haskell
-- x, f x, f (f x), f (f (f x)), ...
iterate f x

-- 2, 4, 8, 16, ...
powersOf2 = iterate (\n -> n^2) 2
```
-}
iterate : (a -> a) -> a -> Stream a
iterate f x =
    cons' <| \_ -> (x, iterate f (f x))

{-| Repeat a value infinitely:

```haskell
-- 1, 1, 1, 1, ...
ones = repeat 1
```
 -}
repeat : a -> Stream a
repeat x = let go = cons x <| \() -> go
           in go

{-| Infinitely cycle through a list, where the head and tail of
the list are given separately to ensure that no one tries to cycle
on an empty list:

```haskell
-- "Alice", "Bob", "Alice", "Bob", ...
cycle "Alice" ["Bob"]
```
-}
cycle : a -> [a] -> Stream a
cycle x xs =
    let cycle' ys = case ys of
                      [] -> go
                      (y :: ys) -> cons y (\_ -> cycle' ys)
        go = cons' <| \_ -> (x, cycle' xs)
    in go

{-| Apply a function to every element of a Stream.

```haskell
-- 2, 2, 2, 2, ...
twos = map (\n -> n + 1) ones
```
-}
map : (a -> b) -> Stream a -> Stream b
map f xs =
    cons' <| \_ -> (f (head xs), map f (tail xs))

{-| Pairwise apply a stream of functions to a stream of arguments.
When paired with `map`, this can be used to emulate `zipWith` over *n* streams.

```haskell
zipWith3 : (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
zipWith3 f xs ys zs = f `S.map` xs `S.apply` ys `S.apply` zs
```
-}
apply : Stream (a -> b) -> Stream a -> Stream b
apply fs xs = zipWith (<|) fs xs

{-| Combine two streams, putting them into tuples pairwise:

```haskell
-- (1,2), (1,4), (1,8), (1,16), ...
pairs = zip ones powersOf2
```
-}
zip : Stream a -> Stream b -> Stream (a,b)
zip = zipWith (\x y -> (x,y))

{-| Combine two streams, applying the given function pairwise:

```haskell
-- 3, 5, 9, 17, ...
zipWith (+) ones powersOf2
```
-}
zipWith : (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f xs ys = cons' <| \() ->
  (f (head xs) (head ys),
   zipWith f (tail xs) (tail ys))

{-| Scan over a stream from the left, building an infinite stream of reductions.

```haskell
-- 0, 1, 2, 3, ...
runningTotal = scanl (+) 0 ones
```
-}
scanl : (a -> b -> b) -> b -> Stream a -> Stream b
scanl f init xs =
    cons' <| \() -> (init, scanl f (f (head xs) init) (tail xs))

{-| Take the first *n* elements of a stream

```haskell
take 5 powersOf2 == [2,4,8,16,32]
```
-}
take : Int -> Stream a -> [a]
take n xs = fst <| splitAt n xs

{-| Drop the first *n* elements of a stream

```haskell
-- 64, 128, 256, 512, ...
drop 5 powersOf2
```
-}
drop : Int -> Stream a -> Stream a
drop n xs = snd <| splitAt n xs

{-| Combination of `take` and `drop`

```haskell
splitAt n xs == (take n xs, drop n xs)
```
-}
splitAt : Int -> Stream a -> ([a], Stream a)
splitAt n xs = case n of
  0 -> ([], xs)
  n -> let (heads, end) = splitAt (n - 1) (tail xs)
       in (head xs :: heads, end)

{-| Turn a stream into a signal of the elements of the stream,
advancing through the stream whenever an event in the given signal is
fired.
-}
sampleOn : Signal b -> Stream a -> Signal a
sampleOn sig str = let tails = foldp (\_ -> tail) str sig in
                   head <~ tails

{-| Filter the elements of a Stream according to a predicate.

```haskell
-- 1, 3, 5, 7, ...
filter isOdd naturals

-- Bad! Infinite loop!
filter isOdd twos
```

The produced Stream can go into an infinite loop if no more elements
satisfy the predicate. So be sure that there are infinitely many terms
in the stream that satisfy the predicate.
-}
filter : (a -> Bool) -> Stream a -> Stream a
filter pred xs = cons' <| (\() ->
  let (hd, tl) = unS xs in
  case pred hd of
    True ->  (hd, filter pred tl)
    False -> unS <| filter pred tl)

{-| Take values from the stream for as long as the predicate holds.
This can infinite loop if all elements of the stream satisfy the predicate!

```haskell
takeWhile (\n -> n < 10) powersOf2 == [2,4,8]

-- Bad! Infinite loop!
takeWhile isEven powersOf2
```
-}
takeWhile : (a -> Bool) -> Stream a -> [a]
takeWhile pred xs = fst <| splitWith pred xs

{-| Drop values from the stream as long as the predicate holds.
This can infinite loop if all elements of the stream satisfy the predicate!

```haskell
-- 16, 32, 64, 128, ...
dropWhile (\n -> n < 10) powersOf2

-- Bad! Infinite loop!
dropWhile isEven powersOf2
```
-}
dropWhile : (a -> Bool) -> Stream a -> Stream a
dropWhile pred xs = snd <| splitWith pred xs

{-| Split a stream when a predicate holds and
the rest of the Stream. This function only terminates if there is
an element of the stream for which the predicate does not hold.

    splitWith pred xs == (takeWhile pred xs, dropWhile pred xs)
-}
splitWith : (a -> Bool) -> Stream a -> ([a], Stream a)
splitWith pred xs = let (hd, tl) = unS xs in
  case pred hd of
    True  -> let (taken, dropped) = splitWith pred tl
             in (hd :: taken, dropped)
    False -> ([], xs)
