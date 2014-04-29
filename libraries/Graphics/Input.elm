module Graphics.Input where
{-| This module is for creating standard input widgets such as buttons and
text fields. All functions in this library follow a general pattern in which
you create an `Input` that many elements can report to:

```haskell
clicks : Input ()
clicks = input ()

clickableYogi : Element
clickableYogi = clickable clicks.handle () (image 40 40 "/yogi.jpg")
```

Whenever the user clicks on the resulting `clickableYogi` element, it sends an
update to the `clicks` input. You will see this pattern again and again in
examples in this library, so just read on to get a better idea of how it works!

# Creating Inputs
@docs Input, input

# Basic Input Elements

To learn about text fields, see the
[`Graphics.Input.Field`](Graphics-Input-Field) library.

@docs button, customButton, checkbox, dropDown

# Clicks and Hovers
@docs clickable, hoverable

-}

import Signal (Signal)
import Graphics.Element (Element)
import Native.Graphics.Input

{-| This is the key abstraction of this library. An `Input` is a record
of two fields:

  1. `signal` &mdash; all values coming to this input from &ldquo;the world&rdquo;
  2. `handle` &mdash; a way to refer to this particular input and send it values

This will make more sense as you see more examples.
-}
type Input a = { signal : Signal a, handle : Handle a }

data Handle a = Handle

{-| This creates a new `Input`. You provide a single argument that will serve
as the initial value of the input&rsquo;s `signal`. For example:

      numbers : Input Int
      numbers = input 42

The initial value of `numbers.signal` is 42, and you will be able
to pipe updates to the input using `numbers.handle`.

Note: This is an inherently impure function. Specifically, `(input ())` and
`(input ())` are actually two different inputs with different signals and handles.
-}
input : a -> Input a
input = Native.Graphics.Input.input

{-| Create a standard button. The following example begins making a basic
calculator:

      data Keys = Number Int | Plus | Minus | Clear

      keys : Input Keys
      keys = input Clear

      calculator : Element
      calculator =
          flow right [ button keys.handle (Number 1) "1"
                     , button keys.handle (Number 2) "2"
                     , button keys.handle    Plus    "+"
                     ]

If the user presses the "+" button, `keys.signal` will update to `Plus`. If the
users presses "2", `keys.signal` will update to `(Number 2)`.
-}
button : Handle a -> a -> String -> Element
button = Native.Graphics.Input.button

{-| Same as `button` but lets you customize buttons to look however you want.

      click : Input ()
      click = input ()

      prettyButton : Element
      prettyButton =
          customButton click.handle ()
              (image 100 40 "/button_up.jpg")
              (image 100 40 "/button_hover.jpg")
              (image 100 40 "/button_down.jpg")
-}
customButton : Handle a -> a -> Element -> Element -> Element -> Element
customButton = Native.Graphics.Input.customButton

{-| Create a checkbox. The following example creates three synced checkboxes:

      check : Input Bool
      check = input False

      boxes : Bool -> Element
      boxes checked =
          let box = container 40 40 middle (checkbox check.handle id checked)
          in  flow right [ box, box, box ]

      main : Signal Element
      main = boxes <~ check.signal
-}
checkbox : Handle a -> (Bool -> a) -> Bool -> Element
checkbox = Native.Graphics.Input.checkbox

{-| Create a drop-down menu.  The following drop-down lets you choose your
favorite British sport:

      data Sport = Football | Cricket | Snooker

      sport : Input (Maybe Sport)
      sport = input Nothing

      sportDropDown : Element
      sportDropDown =
          dropDown sport.handle
            [ (""        , Nothing)
            , ("Football", Just Football)
            , ("Cricket" , Just Cricket)
            , ("Snooker" , Just Snooker)
            ]

If the user selects "Football" from the drop down menue, `sport.signal`
will update to `Just Football`.
-}
dropDown : Handle a -> [(String,a)] -> Element
dropDown = Native.Graphics.Input.dropDown

{-| Create a range slider. The following slider lets you choose a multiple
of 0.5 between -5 and 5, with default value 1:

      slide : Input Float
      slide = input 1

      slideSlider : Signal Element
      slideSlider = slider slide.handle id -5 5 0.5 <~ slide.signal
-}
slider : Handle a -> (Float -> a) -> Float -> Float -> Float -> Float -> Element
slider = Native.Graphics.Input.slider

{-| Detect mouse hovers over a specific `Element`. In the following example,
we will create a hoverable picture called `cat`.

      hover : Input Bool
      hover = input False

      cat : Element
      cat = image 30 30 "/cat.jpg"
              |> hoverable hover.handle id

When the mouse hovers above the `cat` element, `hover.signal` will become
`True`. When the mouse leaves it, `hover.signal` will become `False`.
-}
hoverable : Handle a -> (Bool -> a) -> Element -> Element
hoverable = Native.Graphics.Input.hoverable

{-| Detect mouse clicks on a specific `Element`. In the following example,
we will create a clickable picture called `cat`.

      data Picture = Cat | Hat

      picture : Input Picture
      picture = input Cat

      cat : Element
      cat = image 30 30 "/cat.jpg"
               |> clickable picture.handle Cat

      hat : Element
      hat = image 30 30 "/hat.jpg"
               |> clickable picture.handle Hat

When the user clicks on the `cat` element, `picture.signal` receives
an update containing the value `Cat`. When the user clicks on the `hat` element,
`picture.signal` receives an update containing the value `Hat`. This lets you
distinguish which element was clicked. In a more complex example, they could be
distinguished with IDs or more complex data structures.
-}
clickable : Handle a -> a -> Element -> Element
clickable = Native.Graphics.Input.clickable
