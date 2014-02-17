module Graphics.Input where
{-| This module is for creating standard input widgets such as buttons and
text fields. All functions in this library follow this general pattern:

```haskell
-- (Signal (), SignalID ())
(clicks, clickID) = input ()

clickableYogi : Element
clickableYogi = clickable clickID () (image 40 40 "/yogi.jpg")
```

Whenever the user clicks on the resulting `clickableYogi` element, it sends a
`()` to the `clicks` signal. Typically, you would create all your input signals
when setting up the signal graph, so then your graphics code can refer to them
easily.

# Creating Inputs
@docs input

# Basic Input Elements
Text fields come later.
@docs button, customButton, checkbox, dropDown

# Clicks and Hovers
@docs clickable, hoverable

# Text Fields
@docs field, password, email, noContent, FieldContent, Selection, Direction
-}

import Signal (Signal)
import Graphics.Element (Element)

data SignalID a = SignalID

{-| This function is the key to using this library. It creates a signal that
input elements (buttens, checkboxes, etc.) can report to. It also create a
unique `SignalID` that allows input elements to refer to this particular
signal. It may be best to look at examples in the rest of the docs and return
to this function.

Note: creating an input signal is an inherently imperative action, so this is
one of very few impure functions in Elm. It is designed such that it can only
be used as you build your signal graph at startup, keeping Elm pure at runtime.
-}
input : a -> (Signal a, SignalID a)
input = Native.Graphics.Input.input

{-| Create a standard button. The following example begins making a basic
calculator:

      data Input = Number Int | Plus | Minus | Clear

      -- (Signal Input, SignalID Input)
      (calcInput, pressID) = input Clear

      buttons : Element
      buttons =
          flow right [ button pressID (Number 1) "1"
                     , button pressID (Number 2) "2"
                     , button pressID    Plus    "+"
                     ]

If the user presses the "plus" button, the `calcInput` signal will update to
`Plus`. If the users presses "2", `calcInput` will update to `(Number 2)`.
-}
button : SignalID a -> a -> String -> Element
button = Native.Graphics.Input.button

customButton : SignalID a -> a -> Element -> Element -> Element -> Element
customButton = Native.Graphics.Input.customButton

checkbox : SignalID a -> (Bool -> a) -> Bool -> Element
checkbox = Native.Graphics.Input.checkboxes

{-| Create a drop-down menu.  The following drop-down lets you choose your
favorite British sport:

      data Sport = Football | Cricket | Snooker

      -- (Signal (Maybe Sport), SignalID (Maybe Sport))
      (dropStatus, dropID) = input Nothing

      sportDropDown : Element
      sportDropDown =
          dropDown dropID
            [ (""        , Nothing)
            , ("Football", Just Football)
            , ("Cricket" , Just Cricket)
            , ("Snooker" , Just Snooker)
            ]

If the user selects "Football" from the drop down menue, the `dropStatus`
signal will update to `Just Football`.
-}
dropDown : SignalID a -> [(String,a)] -> Element
dropDown = Native.Graphics.Input.dropDown

{-| Detect mouse hovers over a specific `Element`. In the following example,
we will create a hoverable picture called `cat`.

      -- (Signal Bool, SignalID Bool)
      (hover, hoverID) = input False

      cat : Element
      cat = image 30 30 "/cat.jpg"
              |> hoverable hoverID (\hover -> hover)

When the mouse hovers above the `cat` element, the `hover` signal will become
`True`. When the mouse leaves it, `hover` will become `False`.
-}
hoverable : SignalID a -> (Bool -> a) -> Element -> Element
hoverable = Native.Graphics.Input.hoverable

{-| Detect mouse clicks on a specific `Element`. In the following example,
we will create a clickable picture called `cat`.

      data Picture = Cat | Hat

      -- (Signal Picture, SignalID Picture)
      (click, clickID) = input Cat

      cat : Element
      cat = image 30 30 "/cat.jpg"
               |> clickable clickID Cat

      hat : Element
      hat = image 30 30 "/hat.jpg"
               |> clickable clickID Hat

When the user clicks on the `cat` element, the `click` signal receives
an update containing the value `Cat`. When the user clicks on the `hat` element,
the `click` signal receives an update containing the value `Hat`. This lets you
distinguish which element was clicked. In a more complex example, they could be
distinguished with IDs or more complex data structures.
-}
clickable : SignalID a -> a -> Element -> Element
clickable = Native.Graphics.Input.clickable

{-| Represents the current content of a text field. For example:

        FieldContent "She sells sea shells" (Selection 0 3 Backward)

This means the user highlighted the substring `"She"` backwards.
-}
type FieldContent = { string:String, selection:Selection }

{-| The selection within a text field. `start` is never greater than `end`:

      Selection 0 0 Forward  -- cursor precedes all characters

      Selection 5 9 Backward -- highlighting characters starting after
                             -- the 5th and ending after the 9th
-}
type Selection = { start:Int, end:Int, direction:Direction }

{-| The direction of selection.-}
data Direction = Forward | Backward

{-| A field with no content:

      FieldContent "" (Selection 0 0 Forward)
-}
noContent : FieldContent
noContent = FieldContent "" (Selection 0 0 Forward)

{-| Create a text field. The following example creates a time-varying element
called `nameField`. As the user types their name, the field will be updated
to match what they have entered.

      -- (Signal FieldContent, SignalID FieldContent)
      (nameContent,nameID) = input noContent

      nameField : Signal Element
      nameField = field nameID (\content -> content) "Name" <~ nameContent
-}
field : SignalID a -> (FieldContent -> a) -> String -> FieldContent -> Element
field = Native.Graphics.Input.field

{-| Same as `field` but the UI element blocks out each characters. -}
password : SignalID a -> (FieldContent -> a) -> String -> FieldContent -> Element
password = Native.Graphics.Input.password

{-| Same as `field` but it adds an annotation that this field is for email
addresses. This is helpful for auto-complete and for mobile users who may
get a custom keyboard with an `@` and `.com` button.
-}
email : SignalID a -> (FieldContent -> a) -> String -> FieldContent -> Element
email = Native.Graphics.Input.email

-- area : SignalID a -> (FieldContent -> a) -> SignalID b -> ((Int,Int) -> b) -> (Int,Int) -> String -> FieldContent -> Element
-- area = Native.Graphics.Input.area
