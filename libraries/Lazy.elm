module Lazy ( run, thunk, map, apply, bind
            ) where

{-| Library for efficient Lazy evaluation.

-}

import Basics ((<|), (.))
import Native.Lazy

data Lazy a = L { run : () -> a }

{-| Memoize a thunk so it is evaluated at most once. -}
thunk : (() -> a) -> Lazy a
thunk t = L { run = (Native.Lazy.thunk t) }

{-| Execute a lazy value. -}
run : Lazy a -> a
run (L r) = r.run ()

{-| Lazily apply a pure function to a lazy value. -}
map : (a -> b) -> Lazy a -> Lazy b
map f t = thunk <| \() ->
  f . run <| t

{-| Lazily apply a Lazy function to a Lazy value. -}
apply : Lazy (a -> b) -> Lazy a -> Lazy b
apply f x = thunk <| \() ->
  (run f) (run x)

{-| Lazily chain together Lazy computations. -}
bind : Lazy a -> (a -> Lazy b) -> Lazy b
bind x k = thunk <| \() ->
  run . k . run <| x
