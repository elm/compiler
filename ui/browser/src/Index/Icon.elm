module Index.Icon exposing
  ( home
  , image
  , file
  , gift
  , folder
  , package
  , plus
  , lookup
  )

import Dict
import Html exposing (Html)
import Html.Attributes exposing (class)
import Svg exposing (..)
import Svg.Attributes exposing (width, height, viewBox, d, fill)



-- ICON


icon : String -> String -> String -> Html msg
icon color size path =
  svg
    [ class "icon"
    , width size
    , height size
    , viewBox "0 0 1792 1792"
    ]
    [ path [ fill color, d path ] []
    ]



-- NECESSARY ICONS


home : Html msg
home =
  icon "#babdb6" "36px" "M1472 992v480q0 26-19 45t-45 19h-384v-384h-256v384h-384q-26 0-45-19t-19-45v-480q0-1 .5-3t.5-3l575-474 575 474q1 2 1 6zm223-69l-62 74q-8 9-21 11h-3q-13 0-21-7l-692-577-692 577q-12 8-24 7-13-2-21-11l-62-74q-8-10-7-23.5t11-21.5l719-599q32-26 76-26t76 26l244 204v-195q0-14 9-23t23-9h192q14 0 23 9t9 23v408l219 182q10 8 11 21.5t-7 23.5z"


image : Html msg
image =
  icon "#babdb6" "16px" "M1596 380q28 28 48 76t20 88v1152q0 40-28 68t-68 28h-1344q-40 0-68-28t-28-68v-1600q0-40 28-68t68-28h896q40 0 88 20t76 48zm-444-244v376h376q-10-29-22-41l-313-313q-12-12-41-22zm384 1528v-1024h-416q-40 0-68-28t-28-68v-416h-768v1536h1280zm-128-448v320h-1024v-192l192-192 128 128 384-384zm-832-192q-80 0-136-56t-56-136 56-136 136-56 136 56 56 136-56 136-136 56z"


file : Html msg
file =
  icon "#babdb6" "16px" "M1596 380q28 28 48 76t20 88v1152q0 40-28 68t-68 28h-1344q-40 0-68-28t-28-68v-1600q0-40 28-68t68-28h896q40 0 88 20t76 48zm-444-244v376h376q-10-29-22-41l-313-313q-12-12-41-22zm384 1528v-1024h-416q-40 0-68-28t-28-68v-416h-768v1536h1280zm-1024-864q0-14 9-23t23-9h704q14 0 23 9t9 23v64q0 14-9 23t-23 9h-704q-14 0-23-9t-9-23v-64zm736 224q14 0 23 9t9 23v64q0 14-9 23t-23 9h-704q-14 0-23-9t-9-23v-64q0-14 9-23t23-9h704zm0 256q14 0 23 9t9 23v64q0 14-9 23t-23 9h-704q-14 0-23-9t-9-23v-64q0-14 9-23t23-9h704z"


gift : Html msg
gift =
  icon "#babdb6" "16px" "M1056 1356v-716h-320v716q0 25 18 38.5t46 13.5h192q28 0 46-13.5t18-38.5zm-456-844h195l-126-161q-26-31-69-31-40 0-68 28t-28 68 28 68 68 28zm688-96q0-40-28-68t-68-28q-43 0-69 31l-125 161h194q40 0 68-28t28-68zm376 256v320q0 14-9 23t-23 9h-96v416q0 40-28 68t-68 28h-1088q-40 0-68-28t-28-68v-416h-96q-14 0-23-9t-9-23v-320q0-14 9-23t23-9h440q-93 0-158.5-65.5t-65.5-158.5 65.5-158.5 158.5-65.5q107 0 168 77l128 165 128-165q61-77 168-77 93 0 158.5 65.5t65.5 158.5-65.5 158.5-158.5 65.5h440q14 0 23 9t9 23z"


folder : Html msg
folder =
  icon "#babdb6" "16px" "M1728 608v704q0 92-66 158t-158 66h-1216q-92 0-158-66t-66-158v-960q0-92 66-158t158-66h320q92 0 158 66t66 158v32h672q92 0 158 66t66 158z"


package : Html msg
package =
  icon "#babdb6" "16px" "M1088 832q0-26-19-45t-45-19h-256q-26 0-45 19t-19 45 19 45 45 19h256q26 0 45-19t19-45zm576-192v960q0 26-19 45t-45 19h-1408q-26 0-45-19t-19-45v-960q0-26 19-45t45-19h1408q26 0 45 19t19 45zm64-448v256q0 26-19 45t-45 19h-1536q-26 0-45-19t-19-45v-256q0-26 19-45t45-19h1536q26 0 45 19t19 45z"


plus : Html msg
plus =
  icon "#babdb6" "16px" "M1600 736v192q0 40-28 68t-68 28h-416v416q0 40-28 68t-68 28h-192q-40 0-68-28t-28-68v-416h-416q-40 0-68-28t-28-68v-192q0-40 28-68t68-28h416v-416q0-40 28-68t68-28h192q40 0 68 28t28 68v416h416q40 0 68 28t28 68z"



-- LOOKUP


lookup : String -> Html msg
lookup fileName =
  let
    extension =
      getExtension fileName
  in
    Maybe.withDefault file (Dict.get extension extensionIcons)


extensionIcons : Dict.Dict String (Html msg)
extensionIcons =
  Dict.fromList
    [ ("jpg" , image)
    , ("jpeg", image)
    , ("png" , image)
    , ("gif" , image)
    ]


getExtension : String -> String
getExtension str =
  getExtensionHelp (String.split "." str)


getExtensionHelp : List String -> String
getExtensionHelp segments =
  case segments of
    [] ->
      ""

    [ext] ->
      String.toLower ext

    _ :: rest ->
      getExtensionHelp rest
