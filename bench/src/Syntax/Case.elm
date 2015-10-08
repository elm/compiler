module Syntax.Case (benchmark) where

import Benchmark exposing (Benchmark, test, suite)
import String


benchmark : Benchmark.Benchmark
benchmark =
  suite "case expressions"
    [ suite "simple explicit patterns"
        [ test "Maybe.withDefault 0 (Just 42)" (withDefault 0 (Just 42))
        , test "Maybe.withDefault 0 Nothing" (withDefault 0 Nothing)
        , test "isBlack Diamond" (isBlack Diamond)
        , test "evaluate basicBoolean" (\_ -> evaluate basicBoolean)
        , test "evaluate fancyBoolean" (\_ -> evaluate fancyBoolean)
        ]
    ]


-- 1 Level Deep Patterns

withDefault default maybe _ =
  case maybe of
    Just x ->
        x

    Nothing ->
        default


type Suit = Diamond | Heart | Spade | Club


isBlack suit _ =
  case suit of
    Diamond ->
        False

    Heart ->
        False

    Spade ->
        True

    Club ->
        True


type Boolean
    = Tru
    | Fls
    | And Boolean Boolean
    | Or Boolean Boolean
    | Not Boolean


basicBoolean =
    And (Not Fls) Tru


fancyBoolean =
    Or (Not Tru) (Or Fls (And Tru Tru))


evaluate boolean =
  case boolean of
    Tru ->
        True

    Fls ->
        False

    And a b ->
        evaluate a && evaluate b

    Or a b ->
        evaluate a || evaluate b

    Not bool ->
        not (evaluate bool)
