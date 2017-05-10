port module IncomingPortModule exposing (..)

port incomingPort : (String -> msg) -> Cmd msg
