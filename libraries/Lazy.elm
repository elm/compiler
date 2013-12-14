module Lazy ( force, lazy, map, apply, bind
            ) where

{-| Library for efficient Lazy evaluation.

# Delay
@docs lazy

# Evaluate
@docs force

# Transform
@docs map, apply, bind
-}

import Basics ((<|), (.))
import Native.Lazy

data Lazy a = L { force : () -> a }

{-| Delay a computation. This function memoizes results, so
it guarantees that the computation will be evaluated at most once.
-}
lazy : (() -> a) -> Lazy a
lazy t = L { force = (Native.Lazy.lazy t) }

{-| Evaluate a lazy value. It saves the result so the second time
you call `force` it does not run the computation again. -}
force : Lazy a -> a
force (L r) = r.force ()

{-| Lazily apply a function to a lazy value. The computation
will be delayed until you force the resulting value.
-}
map : (a -> b) -> Lazy a -> Lazy b
map f t = lazy <| \() ->
  f . force <| t

{-| Lazily apply a lazy function to a lazy value. This can
be used to lazily apply a function to many arguments:

```haskell
f `map` a `apply` b `apply` c
```
-}
apply : Lazy (a -> b) -> Lazy a -> Lazy b
apply f x = lazy <| \() ->
  (force f) (force x)

{-| Lazily chain together lazy computations. -}
bind : Lazy a -> (a -> Lazy b) -> Lazy b
bind x k = lazy <| \() ->
  force . k . force <| x
