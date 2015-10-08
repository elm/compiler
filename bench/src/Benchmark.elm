module Benchmark
    ( Benchmark
    , test
    , suite
    , run
    , Statistics
    , view
    )where
{-| A wrapper around benchmark.js that allows benchmarking of pure functions.

# Creating Benchmarks
@docs Benchmark, test, suite

# Run benchmarks
@docs run, Statistics
-}


import Native.Benchmark
import Native.BenchmarkJS
import String
import Task
import Html exposing (Html, div, ul, li, p, text)
import Html.Attributes exposing (style)



-- SET UP BENCHMARK SUITE


{-| A set of benchmarks. -}
type Benchmark
    = Test String Thunk
    | Suite String (List Benchmark)


type Thunk = Thunk


{-| An individual benchmark with a name. It measures how long it takes to
evaluate the given function when it is passed a `()` value.
-}
test : String -> (() -> result) -> Benchmark
test name thunk =
  Test name (Native.Benchmark.makeThunk thunk)


{-| A collection of benchmarks to run together.
-}
suite : String -> List Benchmark -> Benchmark
suite =
  Suite



-- RUN THE BENCHMARKS


{-| Statistics -}
type Statistics
    = StatSuite String (List Statistics)
    | Result String
        { mean : Float
        , hz : Float
        }


{-| Run a benchmark.
-}
run : Benchmark -> Task.Task x Statistics
run =
  Native.Benchmark.run



-- TO HTML


view : Statistics -> Html
view statistics =
  div [ style [] ]
      [ viewHelp statistics
      ]


viewHelp : Statistics -> Html
viewHelp statistics =
  case statistics of
    Result name {hz} ->
        text (name ++ " - " ++ prettyFloat hz ++ " ops/sec")

    StatSuite name subStats ->
        let
            subResults =
                List.map viewHelp subStats
        in
            p []
              [ text name
              , ul [] (List.map (\html -> li [] [html]) subResults)
              ]


prettyFloat : Float -> String
prettyFloat float =
  String.join "," (numberChunker (round float) [])


numberChunker : Int -> List String -> List String
numberChunker n chunks =
  let
    next =
      n // 1000

    chunk =
      n % 1000
  in
    if next == 0 then
      toString chunk :: chunks

    else
      numberChunker next (String.padLeft 3 '0' (toString chunk) :: chunks)
