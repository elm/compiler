module Errors exposing (main)


import Browser
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import String



-- MAIN


main =
  Browser.fullscreen
    { init = \env -> (env.flags, Cmd.none)
    , update = \_ model -> (model, Cmd.none)
    , view = view
    , subscriptions = \_ -> Sub.none
    , onNavigation = Nothing
    }



-- VIEW


view : String -> Browser.View msg
view message =
  { title = "Error"
  , body =
      [ div
          [ style "width" "100%"
          , style "min-height" "100%"
          , style "display" "flex"
          , style "flex-direction" "column"
          , style "align-items" "center"
          , style "background-color" "black"
          , style "color" "rgb(233, 235, 235)"
          , style "font-family" "monospace"
          ]
          [ div
              [ style "display" "block"
              , style "white-space" "pre"
              , style "background-color" "rgb(39, 40, 34)"
              , style "padding" "2em"
              ]
              [ text message
              ]
          ]
      ]
  }
