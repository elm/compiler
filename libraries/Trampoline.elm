module Trampoline where
{-|
This module provides support for trampolining elm code.

It provides a single vital function:
@docs trampoline
-}
import Native.Trampoline
import open Either

{-|
This function allows you to trampoline with a native loop rather than recursively.

Say I have a program with a lot of recursion, or written in continuation passing style.
A naive implementation will overflow the stack after a short amount of time.
However, it is possible with the help of this function to prevent stack overflows.
One needs to create a lazy thunk and provide an evaluation function.

The evaluation function evaluates the thunk, either producing:
 - A new thunk to be evaluated
 - Or a final result

This thunk evaluation loop must be implemented natively,
because if it was implemented recursively, than too much trampolining would overflow the stack.

Here is a *very* simple example:

We might want to write a function that decrements a number untill it reaches 0:

    f n = if | n == 0 -> 0
             | otherwise -> f <| n - 1

But if we then do:

    f 300000

We will get a stack overflow.

Instead, we can make this recursion into a loop:

    f n =
     if | n == 0 -> Right 0
        | otherwise -> Left <| n - 1
    
    trampoline 300000 f

Here is a more realistic example.

Say we want to create a map function:

    map: (a -> b) -> [a] -> [b]
    map f xs =
     case xs of
      (x::xs) -> f x :: map f xs
      [] -> []

This function doesn't work for long lists.  But we can create a trampolined version that trampolines every 100 elements:

    data MapResult a b
     = Done [b]
     | Continue [a] [b]

    tmap: (a -> b) -> [a] -> [b]
    tmap f xs =
     trampoline (tmap' f xs [] 0)
                (\ thunk ->
                    case thunk of
                     Continue input result -> Left <| tmap' f input result 0
                     Done result -> Right result)

    tmap': (a -> b) -> [a] -> [b] -> Int -> MapResult a b
    tmap' f xs r n =
     if | n == 100 -> Continue xs r
        | otherwise ->
         case xs of
          (x::xs) -> tmap' f xs (f x::r) (n + 1)
          [] -> Done <| reverse r
-}
trampoline: thunk -> (thunk -> Either thunk result) -> result
trampoline = Native.Trampoline.trampoline