
module Text where

import open Basics
import Color (Color)
import Graphics.Element (Element, Three, Pos, ElementPrim, Properties)
import Maybe (Maybe)
import JavaScript (JSString)
import Native.Text as Native

data Text = Text

-- Convert a string into text which can be styled and displayed.
toText : String -> Text
toText = Native.toText

-- Set the typeface of some text. The first argument should be a comma separated listing of the desired typefaces
--     "helvetica, arial, sans-serif"
-- Works the same as the CSS font-family property.
typeface : String -> Text -> Text
typeface = Native.typeface

-- Switch to a monospace typeface. Good for code snippets.
monospace : Text -> Text
monospace = Native.monospace

-- Make text big and noticable.
header : Text -> Text
header = Native.header

-- Create a link.
link : String -> Text -> Text
link = Native.link

-- Set the height of text in pixels.
height : Float -> Text -> Text
height = Native.height

-- Set the color of a string.
color : Color -> Text -> Text
color = Native.color

-- Make a string bold.
bold : Text -> Text
bold = Native.bold

-- Italicize a string.
italic : Text -> Text
italic = Native.italic

-- Draw a line above a string.
overline : Text -> Text
overline = Native.overline

-- Underline a string.
underline : Text -> Text
underline = Native.underline

-- Draw a line through a string.
strikeThrough : Text -> Text
strikeThrough = Native.strikeThrough

-- Display justified, styled text.
justified : Text -> Element
justified = Native.justified

-- Display centered, styled text.
centered : Text -> Element
centered = Native.centered

-- Display right justified, styled text.
righted : Text -> Element
righted = Native.righted

-- Display styled text.
text : Text -> Element
text = Native.text

-- Display a plain string.
plainText : String -> Element
plainText = Native.plainText

-- Convert anything to it's textual representation and make it displayable in browser
--
--     asText == text . monospace . show
--
-- Excellent for debugging.
asText : a -> Element
asText = Native.asText