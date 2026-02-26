module IncompletePattern exposing (..)

type Fruit = Apple | Banana | Cherry

name : Fruit -> String
name fruit =
  case fruit of
    Apple -> "apple"
    Banana -> "banana"
