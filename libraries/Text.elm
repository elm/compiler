module Text where

{-| Functions for displaying text

# Creating Text
@docs toText

# Creating Elements
@docs plainText, asText, text, centered, justified, righted

# Formatting
@docs color, typeface, height, link

# Simple Formatting
@docs monospace, bold, italic, underline, overline, strikeThrough

-}

import Basics (..)
import Color (Color)
import Graphics.Element (Element, Three, Pos, ElementPrim, Properties)
import Maybe (Maybe)
import JavaScript (JSString)
import Native.Text

data Text = Text

{-| Convert a string into text which can be styled and displayed. -}
toText : String -> Text
toText = Native.Text.toText

{-| Set the typeface of some text. The first argument should be a comma
separated listing of the desired typefaces:

        "helvetica, arial, sans-serif"

Works the same as the CSS font-family property.
-}
typeface : String -> Text -> Text
typeface = Native.Text.typeface

{-| Switch to a monospace typeface. Good for code snippets. -}
monospace : Text -> Text
monospace = Native.Text.monospace

{-| Create a link. -}
link : String -> Text -> Text
link = Native.Text.link

{-| Set the height of text in pixels. -}
height : Float -> Text -> Text
height = Native.Text.height

{-| Set the color of a string. -}
color : Color -> Text -> Text
color = Native.Text.color

{-| Make a string bold. -}
bold : Text -> Text
bold = Native.Text.bold

{-| Italicize a string. -}
italic : Text -> Text
italic = Native.Text.italic

{-| Draw a line above a string. -}
overline : Text -> Text
overline = Native.Text.overline

{-| Underline a string. -}
underline : Text -> Text
underline = Native.Text.underline

{-| Draw a line through a string. -}
strikeThrough : Text -> Text
strikeThrough = Native.Text.strikeThrough

{-| Display justified, styled text. -}
justified : Text -> Element
justified = Native.Text.justified

{-| Display centered, styled text. -}
centered : Text -> Element
centered = Native.Text.centered

{-| Display right justified, styled text. -}
righted : Text -> Element
righted = Native.Text.righted

{-| Display styled text. -}
text : Text -> Element
text = Native.Text.text

{-| Display a plain string. -}
plainText : String -> Element
plainText = Native.Text.plainText

{-| for internal use only -}
markdown : Element
markdown = Native.Text.markdown

{-| Convert anything to its textual representation and make it displayable in
the browser:

        asText == text . monospace . show

Excellent for debugging.
-}
asText : a -> Element
asText = Native.Text.asText
