module Index.Skeleton exposing
  ( box
  , readmeBox
  )

import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown

import Index.Icon as Icon



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
        [ div [ class "box-footer" ] [ Icon.plus ]
        ]
