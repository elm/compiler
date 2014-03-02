module Text where

{-| Functions for displaying text

# Creating Text
@docs toText

# Creating Elements
@docs leftAligned, rightAligned, centered, justified

# Links and Style
@docs link, Style, style, Line, defaultStyle

# Convenience Functions

There are two convenience functions for creating an `Element` which can be
useful when debugging or prototyping:

@docs plainText, asText

There are also a bunch of functions to set parts of a `Style` individually:

@docs typeface, monospace, height, color, bold, italic, line

-}

import Basics (..)
import String
import Color (Color, black)
import Graphics.Element (Element, Three, Pos, ElementPrim, Properties)
import Maybe (Maybe, Nothing)
import JavaScript (JSString)
import Native.Show
import Native.Text

data Text = Text

data Line = Under | Over | Through

{-| Representation of all the ways you can style `Text`.
-}
type Style =
  { typeface : [String]
  , height   : Maybe Float
  , color    : Color
  , bold     : Bool
  , italic   : Bool
  , line     : Maybe Line
  }

{-| Plain black text. It uses the browsers default typeface and text height.
No decorations are used:

      { typeface = []
      , height = Nothing
      , color = black
      , bold = False
      , italic = False
      , line = Nothing
      }
-}
defaultStyle : Style
defaultStyle =
  { typeface = []
  , height = Nothing
  , color = black
  , bold = False
  , italic = False
  , line = Nothing
  }

{-| Convert a string into text which can be styled and displayed. -}
toText : String -> Text
toText = Native.Text.toText

{-| Set the style of some text.
-}
style : Style -> Text -> Text
style = Native.Text.style

{-| Provide a list of prefered typefaces for some text.

      ["helvetica","arial","sans-serif"]

Not everyone has access to the same typefaces, so rendering will use the first
typeface in the list that is found on the user's computer. If there are no
matches, it will use their default typeface. Works the same as the CSS
font-family property.
-}
typeface : [String] -> Text -> Text
typeface = Native.Text.typeface

{-| Switch to a monospace typeface. Good for code snippets. -}
monospace : Text -> Text
monospace = Native.Text.monospace

{-| Create a link.

      link "http://elm-lang.org" (toText "Elm Website")
-}
link : String -> Text -> Text
link = Native.Text.link

height : Float -> Text -> Text
height = Native.Text.height

color : Color -> Text -> Text
color = Native.Text.color

bold : Text -> Text
bold = Native.Text.bold

italic : Text -> Text
italic = Native.Text.italic

line : Line -> Text -> Text
line = Native.Text.line

leftAligned : Text -> Element
leftAligned = Native.Text.leftAligned

rightAligned : Text -> Element
rightAligned = Native.Text.rightAligned

centered : Text -> Element
centered = Native.Text.centered

justified : Text -> Element
justified = Native.Text.justified

{-| Display a plain string. -}
plainText : String -> Element
plainText str =
    leftAligned (toText str)

{-| for internal use only -}
markdown : Element
markdown = Native.Text.markdown

{-| Convert anything to its textual representation and make it displayable in
the browser:

        asText == text . monospace . show

Excellent for debugging.
-}
asText : a -> Element
asText value =
    leftAligned (monospace (toText (Native.Show.show value)))
