module Errors exposing (main)

import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import String



main =
  Html.programWithFlags
    { init = init
    , update = update
    , view = view
    , subscriptions = always Sub.none
    }



-- MODEL


type alias Model =
  String


init : String -> (Model, Cmd msg)
init errorMessage =
  (errorMessage, Cmd.none)



-- UPDATE


update : msg -> Model -> (Model, Cmd msg)
update _ model =
  (model, Cmd.none)



-- VIEW


view : Model -> Html msg
view model =
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
        (addColors model)
    ]


addColors : String -> List (Html msg)
addColors message =
  message
    |> String.lines
    |> List.concatMap addColorToLine


addColorToLine : String -> List (Html msg)
addColorToLine line =
  flip (++) [ text "\n" ] <|
    if isBreaker line then
      [ colorful "rgb(51, 187, 200)" ("\n\n" ++ line) ]

    else if isBigBreaker line then
      [ colorful "rgb(211, 56, 211)" line ]

    else if isUnderline line then
      [ colorful "#D5200C" line ]

    else if String.startsWith "    " line then
      [ colorful "#9A9A9A" line ]

    else
      processLine line


colorful : String -> String -> Html msg
colorful color msg =
  span [ style "color" color ] [ text msg ]


isBreaker : String -> Bool
isBreaker line =
  String.startsWith "-- " line
  &&
  String.contains "----------" line


isBigBreaker : String -> Bool
isBigBreaker line =
  String.startsWith "===============" line


isUnderline : String -> Bool
isUnderline line =
  String.all (\c -> c == ' ' || c == '^') line


isLineNumber : String -> Bool
isLineNumber string =
  String.all (\c -> c == ' ' || Char.isDigit c) string


processLine : String -> List (Html msg)
processLine line =
  case String.split "|" line of
    [] ->
      [ text line ]

    starter :: rest ->
      if not (isLineNumber starter) then
        [ text line ]

      else
        let
          restOfLine =
            String.join "|" rest

          marker =
            if String.left 1 restOfLine == ">" then
              colorful "#D5200C" ">"

            else
              text " "
        in
          [ colorful "#9A9A9A" (starter ++ "|")
          , marker
          , colorful "#9A9A9A" (String.dropLeft 1 restOfLine)
          ]