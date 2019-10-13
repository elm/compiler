port module Errors exposing (main)


import Browser
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String
import Json.Decode as D
import Elm.Error as Error



-- PORTS


port jumpTo : Error.Region -> Cmd msg



-- MAIN


main =
  Browser.document
    { init = \flags -> (D.decodeValue Error.decoder flags, Cmd.none)
    , update = \region result -> (result, jumpTo region)
    , view = view
    , subscriptions = \_ -> Sub.none
    }


type alias Msg = Error.Region



-- VIEW


view : Result D.Error Error.Error -> Browser.Document Msg
view result =
  { title = "Problem!"
  , body =
      case result of
        Err err ->
          [ text (D.errorToString err) ]

        Ok error ->
          [ viewError error ]
  }


viewError : Error.Error -> Html Msg
viewError error =
  div
    [ style "width" "calc(100% - 4em)"
    , style "min-height" "calc(100% - 4em)"
    , style "font-family" "monospace"
    , style "white-space" "pre-wrap"
    , style "background-color" "black"
    , style "color" "rgb(233,235,235)"
    , style "padding" "2em"
    ]
    (viewErrorHelp error)


viewErrorHelp : Error.Error -> List (Html Msg)
viewErrorHelp error =
  case error of
    Error.GeneralProblem { title, message } ->
      viewHeader title Nothing :: viewMessage message

    Error.ModuleProblems badModules ->
      viewBadModules badModules



-- VIEW HEADER


viewHeader : String -> Maybe Error.Region -> Html Msg
viewHeader title maybeRegion =
  case maybeRegion of
    Nothing ->
      span [ style "color" "rgb(51,187,200)" ]
        [ text <| "-- " ++ title ++ " "
        , text <| String.repeat (76 - String.length title) "-"
        , text <| "\n\n"
        ]

    Just region ->
      span [ style "color" "rgb(51,187,200)" ]
        [ text <| "-- " ++ title ++ " "
        , text <| String.repeat (60 - String.length title) "-"
        , text " "
        , span
            [ style "cursor" "pointer"
            , style "text-decoration" "underline"
            , onClick region
            ]
            [ text "Jump To Problem"
            ]
        , text <| "\n\n"
        ]



-- VIEW BAD MODULES


viewBadModules : List Error.BadModule -> List (Html Msg)
viewBadModules badModules =
  case badModules of
    [] ->
      []

    [badModule] ->
      [viewBadModule badModule]

    a :: b :: cs ->
      viewBadModule a :: viewSeparator a.name b.name :: viewBadModules (b :: cs)


viewBadModule : Error.BadModule -> Html Msg
viewBadModule { problems } =
  span [] (List.map viewProblem problems)


viewProblem : Error.Problem -> Html Msg
viewProblem problem =
  span [] (viewHeader problem.title (Just problem.region) :: viewMessage problem.message)


viewSeparator : String -> String -> Html msg
viewSeparator before after =
  span [ style "color" "rgb(211,56,211)" ]
    [ text <|
        String.padLeft 80 ' ' (before ++ "  ↑    ") ++ "\n" ++
        "====o======================================================================o====\n" ++
        "    ↓  " ++ after ++ "\n\n\n"
    ]



-- VIEW MESSAGE


viewMessage : List Error.Chunk -> List (Html msg)
viewMessage chunks =
  case chunks of
    [] ->
      [ text "\n\n\n" ]

    chunk :: others ->
      let
        htmlChunk =
          case chunk of
            Error.Unstyled string ->
              text string

            Error.Styled style string ->
              span (styleToAttrs style) [ text string ]
      in
      htmlChunk :: viewMessage others


styleToAttrs : Error.Style -> List (Attribute msg)
styleToAttrs { bold, underline, color } =
  addBold bold <| addUnderline underline <| addColor color []


addBold : Bool -> List (Attribute msg) -> List (Attribute msg)
addBold bool attrs =
  if bool then
    style "font-weight" "bold" :: attrs
  else
    attrs


addUnderline : Bool -> List (Attribute msg) -> List (Attribute msg)
addUnderline bool attrs =
  if bool then
    style "text-decoration" "underline" :: attrs
  else
    attrs


addColor : Maybe Error.Color -> List (Attribute msg) -> List (Attribute msg)
addColor maybeColor attrs =
  case maybeColor of
    Nothing ->
      attrs

    Just color ->
      style "color" (colorToCss color) :: attrs


colorToCss : Error.Color -> String
colorToCss color =
  case color of
    Error.Red -> "rgb(194,54,33)"
    Error.RED -> "rgb(252,57,31)"
    Error.Magenta -> "rgb(211,56,211)"
    Error.MAGENTA -> "rgb(249,53,248)"
    Error.Yellow -> "rgb(173,173,39)"
    Error.YELLOW -> "rgb(234,236,35)"
    Error.Green -> "rgb(37,188,36)"
    Error.GREEN -> "rgb(49,231,34)"
    Error.Cyan -> "rgb(51,187,200)"
    Error.CYAN -> "rgb(20,240,240)"
    Error.Blue -> "rgb(73,46,225)"
    Error.BLUE -> "rgb(88,51,255)"
    Error.White -> "rgb(203,204,205)"
    Error.WHITE -> "rgb(233,235,235)"
    Error.Black -> "rgb(0,0,0)"
    Error.BLACK -> "rgb(129,131,131)"
