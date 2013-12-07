
module Lazy where

{-| Library for efficient Lazy evaluation and Infinite Streams.

-}

import Basics ((-))
import Signal ((<~), foldp, Signal)
import Native.Lazy

data Lazy a = L { run : () -> a }

{-| Memoize a thunk so it is evaluated at most once -}
thunk : (() -> a) -> Lazy a
thunk t = L { run = (Native.Lazy.thunk t) }

run : Lazy a -> a
run l = case l of L r -> r.run ()

data Stream a = S { hd : a, tl : Lazy (Stream a) }

hd : Stream a -> a
hd (S r) = r.hd

tl : Stream a -> Stream a
tl (S r) = run (r.tl)

repeat : a -> Stream a
repeat x = S { hd = x, tl = thunk (\() -> repeat x) }

cycle : a -> [a] -> Stream a
cycle x xs = let cycle' ys = case ys of
                  [] -> S { hd = x, tl = thunk (\() -> cycle' xs) }
                  (y :: ys) -> S { hd = y, tl = thunk (\() -> cycle' ys) }
             in cycle' []

map : (a -> b) -> Stream a -> Stream b
map f xs = S { hd = f (hd xs)
             , tl = thunk (\() -> map f (tl xs))
             }

zip : Stream a -> Stream b -> Stream (a, b)
zip = zipWith (\x y -> (x,y))

zipWith : (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f xs ys = S { hd = f (hd xs) (hd ys)
                    , tl = thunk (\() -> zipWith f (tl xs) (tl ys)) }

take : Int -> Stream a -> [a]
take n xs = case n of
  0 -> []
  n -> hd xs :: take (n - 1) (tl xs)

iterate : (a -> a) -> a -> Stream a
iterate f x = S { hd = x
                , tl = thunk (\() -> map f (iterate f x))
                }

observeWhen : Stream a -> Signal b -> Signal a
observeWhen str sig = let tls = foldp (\_ -> tl) str sig in
                      hd <~ tls
