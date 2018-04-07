module Errors exposing (main)


import Browser
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Json.Decode as D



-- MAIN


main : Program D.Value Model ()
main =
  Browser.fullscreen
    { init = \env -> (D.decodeValue decodeExit env.flags, Cmd.none)
    , update = \_ exit -> (exit, Cmd.none)
    , view = view
    , subscriptions = \_ -> Sub.none
    , onNavigation = Nothing
    }



-- MODEL


type alias Model =
  Result D.Error Exit


type alias Exit = ()


decodeExit : D.Decoder Exit
decodeExit =
  D.null ()



-- VIEW


view : Model -> Browser.View msg
view result =
  { title = "Problem!"
  , body =
      case result of
        Err err ->
          [ text (D.errorToString err) ]

        Ok exit ->
          [ viewExit exit ]
  }


viewExit : Exit -> Html msg
viewExit exit =
  div
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
        [ text "TODO"
        ]
    ]
