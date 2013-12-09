module Lazy ( run, thunk
            ) where

{-| Library for efficient Lazy evaluation.

-}

import Native.Lazy

data Lazy a = L { run : () -> a }

{-| Memoize a thunk so it is evaluated at most once -}
thunk : (() -> a) -> Lazy a
thunk t = L { run = (Native.Lazy.thunk t) }

run : Lazy a -> a
run (L r) = r.run ()

