module Text where
{-| A library for styling and displaying text. Whlie the `String` library
focuses on representing and manipulating strings of character strings, the
`Text` library focuses on how those strings should look on screen. It lets
you make text bold or italic, set the typeface, set the text size, etc.

# Creating Text
@docs toText

# Creating Elements

Each of the following functions places `Text` into a box. The function you use
determines the alignment of the text.

@docs leftAligned, rightAligned, centered, justified

# Links and Style
@docs link, Style, style, defaultStyle, Line

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
import Maybe (Maybe(Nothing))
import Native.Show
import Native.Text

data Text = Text

{-| Styles for lines on text. This allows you to add an underline, an overline,
or a strike out text:

      line Under   (toText "underline")
      line Over    (toText "overline")
      line Through (toText "strike out")
-}
data Line = Under | Over | Through

{-| Represents all the ways you can style `Text`. If the `typeface` list is
empty or the `height` is `Nothing`, the users will fall back on their browser's
default settings. The following `Style` is black, 16 pixel tall, underlined, and
Times New Roman (assuming that typeface is available on the user's computer):

      { typeface = [ "Times New Roman", "serif" ]
      , height   = Just 16
      , color    = black
      , bold     = False
      , italic   = False
      , line     = Just Under
      }
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
No decorations are used.

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

{-| Convert a string into text which can be styled and displayed. To show the
string `"Hello World!"` on screen in italics, you could say:

      main = leftAligned (italic (toText "Hello World!"))
-}
toText : String -> Text
toText = Native.Text.toText

{-| Set the style of some text. For example, if you design a `Style` called
`footerStyle` that is specifically for the bottom of your page, you could apply
it to text like this:

      style footerStyle (toText "the old prince / 2007")
-}
style : Style -> Text -> Text
style = Native.Text.style

{-| Provide a list of prefered typefaces for some text.

      ["helvetica","arial","sans-serif"]

Not every browser has access to the same typefaces, so rendering will use the
first typeface in the list that is found on the user's computer. If there are
no matches, it will use their default typeface. This works the same as the CSS
font-family property.
-}
typeface : [String] -> Text -> Text
typeface = Native.Text.typeface

{-| Switch to a monospace typeface. Good for code snippets.

      monospace (toText "foldl (+) 0 [1,2,3]")
-}
monospace : Text -> Text
monospace = Native.Text.monospace

{-| Create a link by providing a URL and the text of the link.

      link "http://elm-lang.org" (toText "Elm Website")
-}
link : String -> Text -> Text
link = Native.Text.link

{-| Set the height of some text.

      height 40 (toText "Title")
-}
height : Float -> Text -> Text
height = Native.Text.height

{-| Set the color of some text.

      color red (toText "Red")
-}
color : Color -> Text -> Text
color = Native.Text.color

{-| Make text bold.

      toText "sometimes you want " ++ bold (toText "emphasis")
-}
bold : Text -> Text
bold = Native.Text.bold

{-| Make text italic.

      toText "make it " ++ italic (toText "important")
-}
italic : Text -> Text
italic = Native.Text.italic

{-| Put lines on text.

      line Under   (toText "underlined")
      line Over    (toText "overlined")
      line Through (toText "strike out")
-}
line : Line -> Text -> Text
line = Native.Text.line

{-| Align text along the left side of the text block. This is sometimes known as
*ragged right*.
-}
leftAligned : Text -> Element
leftAligned = Native.Text.leftAligned

{-| Align text along the right side of the text block. This is sometimes known
as *ragged left*.
-}
rightAligned : Text -> Element
rightAligned = Native.Text.rightAligned

{-| Center text in the text block. There is equal spacing on either side of a
line of text.
-}
centered : Text -> Element
centered = Native.Text.centered

{-| Align text along the left and right sides of the text block. Word spacing is
adjusted to make this possible.
-}
justified : Text -> Element
justified = Native.Text.justified

{-| Display a string with no styling.

      plainText string = leftAligned (toText string)
-}
plainText : String -> Element
plainText str =
    leftAligned (toText str)

{-| for internal use only -}
markdown : Element
markdown = Native.Text.markdown

{-| Convert anything to its textual representation and make it displayable in
the browser. Excellent for debugging.

        asText value = leftAligned (monospace (toText (show value)))
-}
asText : a -> Element
asText value =
    leftAligned (monospace (toText (Native.Show.show value)))
