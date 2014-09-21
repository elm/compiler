module Either where

{-| Represents any data that can take two different types.

# Type and Constructors
@docs Either

# Taking Eithers apart
@docs either, isLeft, isRight

# Eithers and Lists
@docs lefts, rights, partition

-}

import List
import List ((::))

{-| Represents any data that can take two forms. For example, a user ID may be
either an `Int` or a `String`.

This can also be used for error handling `(Either String a)` where
error messages are stored on the left, and the correct values
(&ldquo;right&rdquo; values) are stored on the right.
-}
data Either a b = Left a | Right b

{-| Apply the first function to a `Left` and the second function to a `Right`.
This allows the extraction of a value from an `Either`.

      either (\n -> n + 1) sqrt (Left  4) == 5
      either (\n -> n + 1) sqrt (Right 4) == 2

      map : (a -> b) -> Either err a -> Either err b
      map f e = either Left (\x -> Right (f x)) e
-}
either : (a -> c) -> (b -> c) -> Either a b -> c
either f g e = case e of { Left x -> f x ; Right y -> g y }

{-| True if the value is a `Left`.

      isLeft (Left "Cat") == True
      isLeft (Right 1123) == False
-}
isLeft : Either a b -> Bool
isLeft e = case e of { Left  _ -> True ; _ -> False }

{-| True if the value is a `Right`.

      isRight (Left "Cat") == False
      isRight (Right 1123) == True
-}
isRight : Either a b -> Bool
isRight e = case e of { Right _ -> True ; _ -> False }

{-| Keep only the values held in `Left` values.

      lefts [Left 3, Right 'a', Left 5, Right "eight"] == [3,5]
-}
lefts : [Either a b] -> [a]
lefts es = List.foldr consLeft [] es

{-| Keep only the values held in `Right` values.

      rights [Left 3, Right 'a', Left 5, Right 'b'] == ['a','b']
-}
rights : [Either a b] -> [b]
rights es = List.foldr consRight [] es

{-| Split into two lists, lefts on the left and rights on the right. So we
have the equivalence: `(partition es == (lefts es, rights es))`

      partition [Left 3, Right 'a', Left 5, Right 'b'] == ([3,5],['a','b'])
-}
partition : [Either a b] -> ([a],[b])
partition es = List.foldr consEither ([],[]) es

consLeft e vs =
    case e of
      Left  v -> v::vs
      Right _ -> vs

consRight e vs =
    case e of
      Left  _ -> vs
      Right v -> v::vs

consEither e (ls,rs) =
    case e of
      Left  l -> (l::ls,rs)
      Right r -> (ls,r::rs)
