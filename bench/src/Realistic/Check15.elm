module Realistic.Check15 (benchmark) where
{-| Source: http://elm-lang.org/examples/checkboxes
-}

import Benchmark
import String


benchmark : Benchmark.Benchmark
benchmark =
  Benchmark.suite "check box business logic"
    [ run "toggle boxes" toggleActions
    ]


run : String -> List Action -> Benchmark.Benchmark
run description actions =
  Benchmark.test
    (description ++ " - " ++ toString (List.length actions) ++ " actions")
    (\_ -> List.foldl update initialModel actions)


toggleActions =
  [ Red True
  , Underline True
  , Bold True
  , Bold False
  , Red False
  , Bold True
  , Bold False
  ]


-- MODEL

type alias Model =
  { red : Bool
  , underline : Bool
  , bold : Bool
  }


initialModel =
  Model False False True


-- UPDATE

type Action
  = Red Bool
  | Underline Bool
  | Bold Bool


update action model =
  case action of
    Red bool ->
        { model | red <- bool }

    Underline bool ->
        { model | underline <- bool }

    Bold bool ->
        { model | bold <- bool }

