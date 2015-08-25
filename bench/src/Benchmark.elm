module Benchmark
    ( Never
    , Benchmark
    , Suite (..)
    , BenchStats
    , run
    , runWithProgress
    , bench
    , bench1
    , bench2
    , bench3
    , bench4
    , bench5
    , bench6
    , bench7
    , bench8
    , bench9
    )where
{-| A wrapper around benchmark.js that allows benchmarking
of pure functions to be evaluated.

# Error and Benchmark types
@docs Never, Benchmark, Suite, BenchStats

# Running benchmarks
@docs run, runWithProgress

# Creating benchmarks
@docs bench, bench1, bench2, bench3, bench4, bench5, bench6, bench7, bench8, bench9
-}


import Task
import String
import Native.Benchmark
import Native.BenchmarkJS
import Signal
import Time


{-|
Our error return type: can never be instantiated
because running benchmarks should not fail
|-}
type Never = 
    Never Never


{-|
Opaque type representing a function to be timed
|-}
type Benchmark = 
    Benchmark


{-|
The results of running a benchmark
|-}
type BenchStats = 
  BenchStats 
  { name : String
  , hz : Float
  , marginOfError : Float
  , moePercent : Float
--  , numRunsSampled : Int
  }


{-|
A single or collection of benchmarks
that can be run, generating output
|-}
type Suite = 
      SingleBenchmark Benchmark
    | Suite String (List Benchmark)


{-|
Run a benchmark, generating a list of results for each benchmark.
Results contain a pretty-printed string summary, and a list
of detailed data for each benchmark.
|-}
run 
  :  Suite 
  -> Task.Task Never (String, List BenchStats)
run = runWithProgress Nothing


{-|
Run a benchmark, generating a list of results for each benchmark
and updating a String signal with progress as the benchmarks run
|-}
runWithProgress 
  :  Maybe (Signal.Mailbox String) 
  -> Suite 
  -> Task.Task Never (String, List BenchStats)
runWithProgress maybeMailbox suite = 
  let
    ourTask = 
      case maybeMailbox of
        Nothing -> \_ -> Task.sleep 0
        Just mailbox -> 
          \s ->
            (Signal.send mailbox.address s
            `Task.andThen` \_ -> Task.sleep (5*Time.second))
          -- `Task.andThen` \_ -> Task.keepGoing
  in
    Native.Benchmark.runWithProgress ourTask suite 

{-|
Create a benchmark with the given name
running an arbitrary thunk
|-}
bench : String -> (() -> result) -> Benchmark
bench = Native.Benchmark.makeBenchmark 


{-|
Functions for creating named benchmarks with 1 to 9 arguments
|-}
bench1 : String -> (a -> result) -> a -> Benchmark
bench1 name f a = 
    Native.Benchmark.makeBenchmark name (\_ -> f a)


{-||-}
bench2 : String -> (a -> b -> result) -> a -> b -> Benchmark
bench2 name f a b = 
    Native.Benchmark.makeBenchmark name (\_ -> f a b)


{-||-}
bench3 : String -> (a -> b -> c -> result) -> a -> b -> c -> Benchmark
bench3 name f a b c = 
    Native.Benchmark.makeBenchmark name (\_ -> f a b c)


{-||-}
bench4 : String -> (a -> b -> c -> d -> result) -> a -> b -> c -> d -> Benchmark
bench4 name f a b c d = 
    Native.Benchmark.makeBenchmark name (\_ -> f a b c d)


{-||-}
bench5 : String -> (a -> b -> c -> d -> e -> result) -> a -> b -> c -> d -> e -> Benchmark
bench5 name f a b c d e = 
    Native.Benchmark.makeBenchmark name (\_ -> f a b c d e)


{-||-}
bench6 : 
  String 
  -> (a -> b -> c -> d -> e -> f -> result) 
  -> a 
  -> b 
  -> c 
  -> d 
  -> e 
  -> f 
  -> Benchmark
bench6 name fn a b c d e f = 
    Native.Benchmark.makeBenchmark name (\_ -> fn a b c d e f)


{-||-}
bench7 :
  String
  -> (a -> b -> c -> d -> e -> f -> g -> result)
  -> a 
  -> b 
  -> c 
  -> d 
  -> e 
  -> f 
  -> g
  -> Benchmark
bench7 name fn a b c d e f g = 
    Native.Benchmark.makeBenchmark name (\_ -> fn a b c d e f g)


{-||-}
bench8 : 
  String 
  -> (a -> b -> c -> d -> e -> f -> g -> h -> result)
  -> a 
  -> b 
  -> c 
  -> d 
  -> e 
  -> f 
  -> g
  -> h
  -> Benchmark
bench8 name fn a b c d e f g h = 
    Native.Benchmark.makeBenchmark name  (\_ -> fn a b c d e f g h)

{-||-}
bench9 :
  String 
  -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> result)
  -> a 
  -> b 
  -> c 
  -> d 
  -> e 
  -> f 
  -> g
  -> h
  -> i
  -> Benchmark
bench9 name fn a b c d e f g h i = 
    Native.Benchmark.makeBenchmark name (\_ -> fn a b c d e f g h i)