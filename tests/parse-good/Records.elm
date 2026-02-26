module Records exposing (..)

point = { x = 1, y = 2 }

getX record = record.x

updateX record = { record | x = 10 }

type alias Point = { x : Int, y : Int }
