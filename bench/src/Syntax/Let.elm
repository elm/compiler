module Syntax.Let (benchmark) where

import Benchmark exposing (Benchmark, test, suite)
import String


benchmark : Benchmark.Benchmark
benchmark =
  suite "let expressions"
    [ test "bind numbers, add them" simpleBindings
    , test "tuple destructuring" tupleDestructuring
    , test "record destructuring" recordDestructuring
    ]


simpleBindings _ =
  let
    x = 3
    y = 4
  in
    x + y


tupleDestructuring _ =
  let
    (x,y) = (3,4)
    (a,b) = (1,1)
  in
    x + y + a + b


recordDestructuring _ =
  let
    {x,y} = { x = 3, y = 4 }
    {a,b} = { a = 1, b = 1 }
  in
    x + y + a + b
