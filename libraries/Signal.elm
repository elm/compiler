
module Signal where

import Native.Signal as Native

constant : a -> Signal a

lift  : (a -> b) -> Signal a -> Signal b
lift2 : (a -> b -> c) -> Signal a -> Signal b -> Signal c
lift3 : (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d
lift4 : (a -> b -> c -> d -> e) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
lift5 : (a -> b -> c -> d -> e -> f) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f
lift6 : (a -> b -> c -> d -> e -> f -> g) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal g
lift7 : (a -> b -> c -> d -> e -> f -> g -> h) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal g -> Signal h
lift8 : (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal g -> Signal h -> Signal i

foldp : (a -> b -> b) -> b -> Signal a -> Signal b

merge : Signal a -> Signal a -> Signal a
merges : [Signal a] -> Signal a
mergeEither : Signal a -> Signal b -> Signal (Either a b)

count : Signal a -> Signal Int
countIf : (a -> Bool) -> Signal a -> Signal Int

keepIf : (a -> Bool) -> a -> Signal a -> Signal a
dropIf : (a -> Bool) -> a -> Signal a -> Signal a
keepWhen : Signal Bool -> a -> Signal a -> Signal a
dropWhen : Signal Bool -> a -> Signal a -> Signal a
dropRepeats : Signal a -> Signal a

sampleOn : Signal a -> Signal b -> Signal b

timestamp : Signal a -> Signal (Time, a)