module Graphics.Input.Field where
{-| This library provides an API for creating and updating text fields.
Text fields use exactly the same approach as [`Graphics.Input`](Graphics-Input)
for modelling user input, allowing you to keep track of new events and update
text fields programmatically.

# Create Fields
@docs field, password, email

# Field Content
@docs Content, Selection, Direction, noContent

# Field Style
@docs defaultStyle, Style, Outline, noOutline, Highlight, noHighlight, Dimensions, uniformly
-}

import Color (Color)
import Color
import Graphics.Element (Element)
import Graphics.Input (Input, Handle)
import Native.Graphics.Input
import Text

{-| Create uniform dimensions:

      uniformly 4 == { left=4, right=4, top=4, bottom=4 }

The following example creates an outline where the left, right, top, and bottom
edges all have width 1:

      Outline grey (uniformly 1) 4
-}
uniformly : Int -> Dimensions
uniformly n = Dimensions n n n n

{-| For setting dimensions of a field's padding or outline. The left, right,
top, and bottom may all have different sizes. The following example creates
dimensions such that the left and right are twice as wide as the top and bottom:

      myDimensions : Int -> Dimensions
      myDimensions n = { left = 2 * n, right = 2 * n, top = n, bottom = n }
-}
type alias Dimensions =
    { left:Int
    , right:Int
    , top:Int
    , bottom:Int
    }

{-| A field can have a outline around it. This lets you set its color, width,
and radius. The radius allows you to round the corners of your field. Set the
width to zero to make it invisible. Here is an example outline that is grey
and thin with slightly rounded corners:

      { color = grey, width = uniformly 1, radius = 4 }
-}
type alias Outline =
    { color:Color
    , width:Dimensions
    , radius:Int
    }

{-| An outline with zero width, so you cannot see it. -}
noOutline : Outline
noOutline = Outline Color.grey (uniformly 0) 0

{-| When a field has focus, it has a blue highlight around it by default. The
`Highlight` lets you set the `color` and `width` of this highlight. Set the
`width` to zero to turn the highlight off. Here is an example highlight that
is blue and thin:

      { color = blue, width = 1 }
-}
type alias Highlight =
    { color:Color
    , width:Int
    }

{-| An highlight with zero width, so you cannot see it. -}
noHighlight : Highlight
noHighlight = Highlight Color.blue 0

{-| Describe the style of a text box. `style` describes the style of the text
itself using [`Text.Style`](/Text#Style). `highlight` describes the glowing blue
highlight that shows up when the field has focus. `outline` describes the line
surrounding the text field, and `padding` adds whitespace between the `outline`
and the text.

The width and height of the text box *includes* the `padding` and `outline`.
Say we have a text box that is 40 pixels tall. It has a uniform outline of
1 pixel and a uniform padding of 5 pixels. Both of these must be subtracted
from the total height to determine how much room there is for text. The
`padding` and `outline` appear on the top and bottom, so there will be 28
vertical pixels remaining for the text (40 - 1 - 5 - 5 - 1).
-}
type alias Style =
    { padding   : Dimensions
    , outline   : Outline
    , highlight : Highlight
    , style     : Text.Style
    }

{-| The default style for a text field. The outline is `Color.grey` with width
1 and radius 2. The highlight is `Color.blue` with width 1, and the default
text color is black.
-}
defaultStyle : Style
defaultStyle =
    { padding   = uniformly 4
    , outline   = Outline Color.grey (uniformly 1) 2
    , highlight = Highlight Color.blue 1
    , style     = Text.defaultStyle
    }

{-| Represents the current content of a text field. For example:

      content = Content "She sells sea shells" (Selection 0 3 Backward)

This means the user highlighted the substring `"She"` backwards. The value of
`content.string` is `"She sells sea shells"`.
-}
type alias Content =
    { string:String
    , selection:Selection
    }

{-| The selection within a text field. `start` is never greater than `end`:

      Selection 0 0 Forward  -- cursor precedes all characters

      Selection 5 9 Backward -- highlighting characters starting after
                             -- the 5th and ending after the 9th
-}
type alias Selection =
    { start:Int
    , end:Int
    , direction:Direction
    }

{-| The direction of selection. When the user highlights a selection in a text
field, they must do it in a particular direction. This determines which end of
the selection moves when they change the selection by pressing Shift-Left or
Shift-Right.
-}
type Direction = Forward | Backward

{-| A field with no content:

      Content "" (Selection 0 0 Forward)
-}
noContent : Content
noContent = Content "" (Selection 0 0 Forward)

{-| Create a text field. The following example creates a time-varying element
called `nameField`. As the user types their name, the field will be updated
to match what they have entered.

      name : Input Content
      name = input noContent

      nameField : Signal Element
      nameField = field defaultStyle name.handle identity "Name" <~ name.signal

When we use the `field` function, we first give it a visual style. This is
the first argument so that it is easier to define your own custom field
(`myField = field myStyle`). The next two arguments are a `Handle` and a
handler function that processes or augments events before sending them along
to the associated `Input`. In the example above we use the `id` function to
pass events along unchanged to the `name` `Input`. We then provide the
place-holder message to use when no input has been provided yet. Finally,
we give the current `Content` of the field. This argument is last because
it is most likely to change frequently, making function composition easier.
-}
field : Style -> Handle a -> (Content -> a) -> String -> Content -> Element
field = Native.Graphics.Input.field

{-| Same as `field` but the UI element blocks out each characters. -}
password : Style -> Handle a -> (Content -> a) -> String -> Content -> Element
password = Native.Graphics.Input.password

{-| Same as `field` but it adds an annotation that this field is for email
addresses. This is helpful for auto-complete and for mobile users who may
get a custom keyboard with an `@` and `.com` button.
-}
email : Style -> Handle a -> (Content -> a) -> String -> Content -> Element
email = Native.Graphics.Input.email

-- area : Handle a -> (Content -> a) -> Handle b -> ((Int,Int) -> b) -> (Int,Int) -> String -> Content -> Element
-- area = Native.Graphics.Input.area
