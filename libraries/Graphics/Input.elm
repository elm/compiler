
module Graphics.Input where

import Signal (lift)
import Native.Graphics.Input as N

id x = x

buttons : a -> { button : a -> String -> Element, events : Signal a }

-- Create a button with a given label. The result is an `Element` and
-- a signal of units. This signal triggers whenever the button is pressed.
button : String -> (Element, Signal ())
button txt =
    let pool = N.buttons ()
    in  (pool.button () txt, pool.events)

customButtons : a -> { button : a -> Element -> Element -> Element -> Element,
                       events : Signal a }

-- Create a button with custom states for up, hovering, and down
-- (given in that order). The result is an `Element` and
-- a signal of units. This signal triggers whenever the button is pressed.
customButton : Element -> Element -> Element -> (Element, Signal ())
customButton up hover down =
    let pool = N.customButtons ()
    in  (pool.button () up hover down, pool.events)

checkboxes : a -> { box : (Bool -> a) -> Bool -> Element, events : Signal a }

-- Create a checkbox with a given start state. Notice that the result is a *signal*
-- of elements. That is because a checkbox has state that updates based on user input.
-- The boolean signal represents the current state of the checkbox.
checkbox : Bool -> (Signal Element, Signal Bool)
checkbox b =
    let cbs = N.checkboxes b
    in  (lift (cbs.box id) cbs.events, cbs.events)

type FieldState = { string:String, start:Int, end:Int }

fields : a -> { field : (FieldState -> a) -> String -> FieldState -> Element,
                events : Signal a }

empty = { string="", start=0, end=0 }

-- Create a field with the given default text. The output is an element that
-- updates to match the user input and a signal of strings representing the
-- content of the field.
field : String -> (Signal Element, Signal String)
field placeHolder =
    let tfs = N.fields empty
    in  (lift (tfs.field id placeHolder) tfs.events, lift .string tfs.events)

-- Same as `field` but the UI element blocks out each characters.
password : String -> (Signal Element, Signal String)
password placeHolder =
    let tfs = N.passwords empty
    in  (lift (tfs.field id placeHolder) tfs.events, lift .string tfs.events)

-- Same as `field` but it adds an annotation that this field is for email addresses.
-- This is helpful for auto-complete and for mobile users who may get a custom keyboard
-- with an `@` and `.com` button.
email : String -> (Signal Element, Signal String)
email placeHolder =
    let tfs = N.emails empty
    in  (lift (tfs.field id placeHolder) tfs.events, lift .string tfs.events)
