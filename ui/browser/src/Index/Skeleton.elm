module Index.Skeleton exposing
  ( view
  , box
  , readmeBox
  )

import Color exposing (Color, darkGrey)
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown

import TempFontAwesome as FA



-- VIEW


view : List (Html msg) -> Html msg
view content =
  div []
    [ header [ class "header" ] []
    , div [class "content"] content
    ]



-- VIEW BOXES


type alias BoxArgs msg =
  { title : String
  , items : List (List (Html msg))
  , footer : Maybe (String, String)
  }


box : BoxArgs msg -> Html msg
box { title, items, footer } =
  let
    realItems =
      List.map (div [ class "box-item" ]) items
  in
    boxHelp title realItems footer


readmeBox : String -> Html msg
readmeBox markdown =
  let
    readme =
      Markdown.toHtml [ class "box-item" ] markdown
  in
    boxHelp "README" [readme] Nothing


boxHelp : String -> List (Html msg) -> Maybe (String, String) -> Html msg
boxHelp boxTitle items footer =
  div [ class "box" ] <|
    div [ class "box-header" ] [ text boxTitle ]
    :: items
    ++ [ boxFooter footer ]


boxFooter : Maybe (String, String) -> Html msg
boxFooter maybeFooter =
  case maybeFooter of
    Nothing ->
      text ""

    Just (path, description) ->
      a [ href path
        , title description
        ]
        [ div [ class "box-footer" ] [ FA.plus darkGrey 14 ]
        ]
