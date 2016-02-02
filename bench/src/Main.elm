module Main where

import Html exposing (Html, text)
import Signal
import Task

import Benchmark
import Syntax.Case as Case
import Syntax.Let as Let
import Syntax.Records as Records
import Realistic.Check as Check
import Realistic.Todo as Todo


-- COMPILER BENCHMARK

compilerBenchmark : Benchmark.Benchmark
compilerBenchmark =
  Benchmark.suite "generated code"
    [ Benchmark.suite "business logic from the wild"
        [ Check.benchmark
        , Todo.benchmark
        ]
    , Benchmark.suite "syntax"
        [ Case.benchmark
        , Let.benchmark
        , Records.benchmark
        ]
    ]


-- DISPLAY THINGS

main =
  results.signal


results : Signal.Mailbox Html
results =
  Signal.mailbox (text "Working...")


port benchResults : Task.Task x ()
port benchResults =
  Benchmark.run compilerBenchmark
    `Task.andThen` (Benchmark.view >> Signal.send results.address)
