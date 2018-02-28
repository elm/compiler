module Index.Navigator exposing (view, (</>))


import Color
import Html exposing (..)
import Html.Attributes exposing (..)

import TempFontAwesome as FA



-- ADD SLASHES


(</>) : String -> String -> String
(</>) directory file =
  if String.endsWith "/" directory then
    directory ++ file

  else
    directory ++ "/" ++ file



-- VIEW


view : String -> List String -> Html msg
view root dirs =
  div
    [ style "font-size" "2em"
    , style "padding" "20px 0"
    , style "display" "flex"
    , style "align-items" "center"
    , style "height" "40px"
    ]
    (makeLinks root dirs "/" [])


makeLinks : String -> List String -> String -> List (Html msg) -> List (Html msg)
makeLinks root dirs oldPath revAnchors =
  case dirs of
    dir :: otherDirs ->
      let
        newPath =
          oldPath </> dir

        anchor =
          a [ href newPath ] [ text dir ]
      in
        makeLinks root otherDirs newPath (anchor :: revAnchors)

    [] ->
      let
        home =
          a [ href "/"
            , title root
            , style "display" "inherit"
            ]
            [ FA.home Color.darkGrey 36
            ]
      in
        case revAnchors of
          [] ->
            [home]

          lastAnchor :: otherRevAnchors ->
            home :: slash :: List.foldl addSlash [lastAnchor] otherRevAnchors


addSlash : Html msg -> List (Html msg) -> List (Html msg)
addSlash front back =
  front :: slash :: back


slash : Html msg
slash =
  span [ style "padding" "0 8px" ] [ text "/" ]
