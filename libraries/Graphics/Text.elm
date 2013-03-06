
module Graphics.Text where

import Native.Graphics.Text as T


toText : String -> Text

typeface  : String -> Text -> Text
href      : String -> Text -> Text
height    : Float -> Text -> Text
color     : Color -> Text -> Text
bold      : Text -> Text
italic    : Text -> Text
overline  : Text -> Text
underline : Text -> Text
strikeThrough : Text -> Text

justified : Text -> Element
centered  : Text -> Element
righted   : Text -> Element
text      : Text -> Element

asText : a -> Element
