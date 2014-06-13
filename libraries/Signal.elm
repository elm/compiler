module Signal where
{-| The library for general signal manipulation. Includes lift functions up to
`lift8` and infix lift operators `<~` and `~`, combinations, filters, and
past-dependence.

Signals are time-varying values. Lifted functions are reevaluated whenever any of
their input signals has an event. Signal events may be of the same value as the
previous value of the signal. Such signals are useful for timing and
past-dependence.

Some useful functions for working with time (e.g. setting FPS) and combining
signals and time (e.g.  delaying updates, getting timestamps) can be found in
the `Time` library.

# Combine
@docs constant, lift, lift2, merge, merges, combine

# Past-Dependence
@docs foldp, count, countIf

#Filters
@docs keepIf, dropIf, keepWhen, dropWhen, dropRepeats, sampleOn

# Pretty Lift
@docs (<~), (~)

# Do you even lift?
@docs lift3, lift4, lift5, lift6, lift7, lift8

-}

import Native.Signal
import List (foldr, (::))
import Basics (fst, snd, not)

data Signal a = Signal

{-| Create a constant signal that never changes. -}
constant : a -> Signal a
constant = Native.Signal.constant

{-| Transform a signal with a given function. -}
lift  : (a -> b) -> Signal a -> Signal b
lift = Native.Signal.lift

{-| Combine two signals with a given function. -}
lift2 : (a -> b -> c) -> Signal a -> Signal b -> Signal c
lift2 = Native.Signal.lift2

lift3 : (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d
lift3 = Native.Signal.lift3

lift4 : (a -> b -> c -> d -> e) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
lift4 = Native.Signal.lift4

lift5 : (a -> b -> c -> d -> e -> f) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f
lift5 = Native.Signal.lift5

lift6 : (a -> b -> c -> d -> e -> f -> g)
      -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal g
lift6 = Native.Signal.lift6

lift7 : (a -> b -> c -> d -> e -> f -> g -> h)
      -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal g -> Signal h
lift7 = Native.Signal.lift7

lift8 : (a -> b -> c -> d -> e -> f -> g -> h -> i)
      -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal g -> Signal h -> Signal i
lift8 = Native.Signal.lift8


{-| Create a past-dependent signal. Each value given on the input signal will
be accumulated, producing a new output value.

For instance, `foldp (+) 0 (fps 40)` is the time the program has been running,
updated 40 times a second. -}
foldp : (a -> b -> b) -> b -> Signal a -> Signal b
foldp = Native.Signal.foldp

{-| Merge two signals into one, biased towards the first signal if both signals
update at the same time. -}
merge : Signal a -> Signal a -> Signal a
merge = Native.Signal.merge

{-| Merge many signals into one, biased towards the left-most signal if multiple
signals update simultaneously. -}
merges : [Signal a] -> Signal a
merges = Native.Signal.merges

{-| Combine a list of signals into a signal of lists. -}
combine : [Signal a] -> Signal [a]
combine = foldr (Native.Signal.lift2 (::)) (Native.Signal.constant [])

 -- Merge two signals into one, but distinguishing the values by marking the first
 -- signal as `Left` and the second signal as `Right`. This allows you to easily
 -- fold over non-homogeneous inputs.
 -- mergeEither : Signal a -> Signal b -> Signal (Either a b)

{-| Count the number of events that have occurred. -}
count : Signal a -> Signal Int
count = Native.Signal.count

{-| Count the number of events that have occurred that satisfy a given predicate.
-}
countIf : (a -> Bool) -> Signal a -> Signal Int
countIf = Native.Signal.countIf

{-| Keep only events that satisfy the given predicate. Elm does not allow
undefined signals, so a base case must be provided in case the predicate is
not satisfied initially. -}
keepIf : (a -> Bool) -> a -> Signal a -> Signal a
keepIf = Native.Signal.keepIf

{-| Drop events that satisfy the given predicate. Elm does not allow undefined
signals, so a base case must be provided in case the predicate is satisfied
initially. -}
dropIf : (a -> Bool) -> a -> Signal a -> Signal a
dropIf = Native.Signal.dropIf

{-| Keep events only when the first signal is true. Elm does not allow undefined
signals, so a base case must be provided in case the first signal is not true
initially.
-}
keepWhen : Signal Bool -> a -> Signal a -> Signal a
keepWhen bs def sig = 
  snd <~ (keepIf fst (False, def) ((,) <~ (sampleOn sig bs) ~ sig))

{-| Drop events when the first signal is true. Elm does not allow undefined
signals, so a base case must be provided in case the first signal is true
initially.
-}
dropWhen : Signal Bool -> a -> Signal a -> Signal a
dropWhen bs = keepWhen (not <~ bs)

{-| Drop updates that repeat the current value of the signal.

Imagine a signal `numbers` has initial value
0 and then updates with values 0, 0, 1, 1, and 2. `dropRepeats numbers`
is a signal that has initial value 0 and updates as follows: ignore 0,
ignore 0, update to 1, ignore 1, update to 2. -}
dropRepeats : Signal a -> Signal a
dropRepeats = Native.Signal.dropRepeats

{-| Sample from the second input every time an event occurs on the first input.
For example, `(sampleOn clicks (every second))` will give the approximate time
of the latest click. -}
sampleOn : Signal a -> Signal b -> Signal b
sampleOn = Native.Signal.sampleOn

{-| An alias for `lift`. A prettier way to apply a function to the current value
of a signal. -}
(<~) : (a -> b) -> Signal a -> Signal b
f <~ s = Native.Signal.lift f s

{-| Informally, an alias for `liftN`. Intersperse it between additional signal
arguments of the lifted function.

Formally, signal application. This takes two signals, holding a function and
a value. It applies the current function to the current value.

The following expressions are equivalent:

         scene <~ Window.dimensions ~ Mouse.position
         lift2 scene Window.dimensions Mouse.position
-}
(~) : Signal (a -> b) -> Signal a -> Signal b
sf ~ s = Native.Signal.lift2 (\f x -> f x) sf s

infixl 4 <~
infixl 4 ~
