module NotFound exposing (main)


import Html exposing (..)
import Html.Attributes exposing (..)



main : Html msg
main =
  div [ class "not-found" ]
    [ div [ style "font-size" "12em" ] [ text "404" ]
    , div [ style "font-size" "3em" ] [ text "Page not found" ]
    ]
