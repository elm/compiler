module Trampoline where
{-| A [trampoline](http://en.wikipedia.org/wiki/Tail-recursive_function#Through_trampolining)
makes it possible to recursively call a function without growing the stack.

Popular JavaScript implementations do not perform any tail-call elimination, so
recursive functions can cause a stack overflow if they go to deep. Trampolines
permit unbounded recursion despite limitations in JavaScript.

This strategy may create many intermediate closures, which is very expensive in
JavaScript, so use this library only when it is essential that you recurse deeply.

# Trampolines
@docs trampoline, Trampoline
-}

import Native.Trampoline

{-| A way to build computations that may be deeply recursive. We will take an
example of a tail-recursive function and rewrite it in a way that lets us use
a trampoline:

      length : [a] -> Int
      length list = length' 0 list

      length' : Int -> [a] -> Int
      length' accum list =
          case list of
            []     -> accum
            hd::tl -> length' (accum+1) tl

This finds the length of a list, but if the list is too long, it may cause a
stack overflow. We can rewrite it as follows:

      length : [a] -> Int
      length list = trampoline (length' 0 list)

      length' : Int -> [a] -> Trampoline Int
      length' accum list =
          case list of
            []     -> Done accum
            hd::tl -> Continue (\() -> length' (accum+1) tl)

Now it uses a trampoline and can recurse without growing the stack!
-}
type Trampoline a
    = Done a
    | Continue (() -> Trampoline a)

{-| Evaluate a trampolined value in constant space. -}
trampoline : Trampoline a -> a
trampoline = Native.Trampoline.trampoline
