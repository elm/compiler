module Benchmark
    ( Suite
    , bench
    , suite
    , run
    , Statistics
    , statisticsToString
    )where
{-| A wrapper around benchmark.js that allows benchmarking of pure functions.

# Creating Benchmarks
@docs Benchmark, bench, suite

# Run benchmarks
@docs run, Statistics
-}


import Native.Benchmark
import Native.BenchmarkJS
import String
import Task



-- SET UP BENCHMARK SUITE


{-| A set of benchmarks. -}
type Benchmark
    = Test String Thunk
    | Suite String (List Benchmark)


type Thunk = Thunk


{-| An individual benchmark with a name. It measures how long it takes to
evaluate the given function when it is passed a `()` value.
-}
bench : String -> (() -> result) -> Benchmark
bench name thunk =
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
        }


{-| Run a benchmark.
-}
run : Benchmark -> Task.Task x Statistics
run =
  Native.Benchmark.run



-- TO STRING


statisticsToString : Statistics -> String
statisticsToString statistics =
  List.join "\n" (statisticsToStringHelp statistics)


statisticsToStringHelp : Statistics -> (List String, Float)
statisticsToStringHelp statistics =
  case statistics of
    Result name {mean} ->
        ( [ name ++ " - " ++ toString mean ]
        , mean
        )

    StatSuite name subStats ->
        let
            (strings, subMeans) =
                List.unzip (List.map statisticsToStringHelp subStats)

            total =
                List.sum means
        in
            ( name ++ " - " ++ toString total
              :: List.map (String.padLeft 4 ' ') (List.concat strings)
            , total
            )
