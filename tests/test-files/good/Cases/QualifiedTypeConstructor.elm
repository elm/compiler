import Foobar exposing (..)

type Action = Action Foobar

count action =
  case action of
    Action Foobar.Foo -> 0
    Action Foobar.Bar -> 1
