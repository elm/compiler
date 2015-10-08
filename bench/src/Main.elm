module Main where

import Graphics.Element exposing (leftAligned)
import Signal
import Task
import Text

import Benchmark
import Realistic.Todo as Todo


-- COMPILER BENCHMARK

compilerBenchmark : List Benchmark.Benchmark
compilerBenchmark =
  [ Todo.benchmark
  ]


-- DISPLAY THINGS

main =
  Signal.map (leftAligned << Text.fromString) results.signal


results : Signal.Mailbox String
results =
  Signal.mailbox "Working..."


port benchResults : Task.Task x ()
port benchResults =
  Benchmark.run compilerBenchmark
    `Task.andThen` (Benchmark.statisticsToString >> Signal.send results.address)
