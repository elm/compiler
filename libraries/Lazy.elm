module Lazy ( force, lazy, map, apply, bind
            ) where

{-| Library for efficient Lazy evaluation.

-}

import Basics ((<|), (.))
import Native.Lazy

data Lazy a = L { force : () -> a }

{-| Memoize a thunk so it is evaluated at most once. -}
lazy : (() -> a) -> Lazy a
lazy t = L { force = (Native.Lazy.lazy t) }

{-| Execute a lazy value. -}
force : Lazy a -> a
force (L r) = r.force ()

{-| Lazily apply a pure function to a lazy value. -}
map : (a -> b) -> Lazy a -> Lazy b
map f t = lazy <| \() ->
  f . force <| t

{-| Lazily apply a Lazy function to a Lazy value. -}
apply : Lazy (a -> b) -> Lazy a -> Lazy b
apply f x = lazy <| \() ->
  (force f) (force x)

{-| Lazily chain together Lazy computations. -}
bind : Lazy a -> (a -> Lazy b) -> Lazy b
bind x k = lazy <| \() ->
  force . k . force <| x
