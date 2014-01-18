module Graphics.Input where

{-| This module is for creating standard input widgets such as buttons and
text boxes. In general, functions in this library return a signal representing
events from the user.

The simplest inputs are *one-way inputs*, meaning the user can update
them, but the programmer cannot. If you need to update an input from
within the program you want the slightly more complex *two-way inputs*.
This document will always show the one-way inputs first, *then* the
two-way inputs.

# Buttons
@docs button, customButton, buttons, customButtons

# Fields
@docs field, password, email, fieldMultiline, fields, fieldsMultiline, FieldState, emptyFieldState

# Checkboxes
@docs checkbox, checkboxes

# Drop Downs
@docs stringDropDown, dropDown

# Mouse Hover
@docs hoverable, hoverables
-}

import Basics (String)
import Signal (Signal,lift,dropRepeats)
import Native.Graphics.Input
import List
import Graphics.Element (Element)
import Maybe (Maybe)
import JavaScript (JSString)

id x = x

{-| Create a group of buttons.

 * The first argument is the default value of the `events` signal.
 * The `events` signal represents all of the activity in this group
   of buttons.
 * The `button` function creates a button
   with the given name, like &ldquo;Submit&rdquo; or &ldquo;Cancel&rdquo;.
   The `a` value is sent to `events` whenever the button is pressed.
-}
buttons : a -> { events : Signal a,
                 button : a -> String -> Element }
buttons = Native.Graphics.Input.buttons

{-| Create a button with a given label. The result is an `Element` and
a signal of units. This signal triggers whenever the button is pressed.
-}
button : String -> (Element, Signal ())
button txt =
    let pool = buttons ()
    in  (pool.button () txt, pool.events)

{-| Create a group of custom buttons.

 * The first argument is the default value of the `events` signal.
 * The `events` signal represents all of the activity in this group
   of custom buttons.
 * The `customButton` function creates a button with three different visual
   states, one for up, hovering, and down. The resulting button has dimensions
   large enough to fit all three possible `Elements`.
   The `a` value is sent to `events` whenever the button is pressed.
-}
customButtons : a -> { events : Signal a,
                       customButton : a -> Element -> Element -> Element -> Element }
customButtons = Native.Graphics.Input.customButtons

{-| Create a button with custom states for up, hovering, and down
(given in that order). The result is an `Element` and  a signal of
units. This signal triggers whenever the button is pressed.
-}
customButton : Element -> Element -> Element -> (Element, Signal ())
customButton up hover down =
    let pool = customButtons ()
    in  (pool.customButton () up hover down, pool.events)

{-| Create a group of checkboxes.

 * The first argument is the default value of the `events` signal.
 * The `events` signal represents all of the activity in this group
   of checkboxes.
 * The `checkbox` function creates a
   checkbox with a given state. The `(Bool -> a)` function is used
   when the checkbox is modified. It takes the new state and turns
   it into a value that can be sent to `events`. For example, this
   lets you add an ID to distinguish between checkboxes.
-}
checkboxes : a -> { events : Signal a,
                    checkbox : (Bool -> a) -> Bool -> Element }
checkboxes = Native.Graphics.Input.checkboxes

{-| Create a checkbox with a given start state. Unlike `button`, this
result is a *signal* of elements. That is because a checkbox has state
that updates based on user input. The boolean signal represents the
current state of the checkbox.
-}
checkbox : Bool -> (Signal Element, Signal Bool)
checkbox b =
    let cbs = checkboxes b
    in  (lift (cbs.checkbox id) cbs.events, cbs.events)

{-| Detect when the mouse is hovering over some elements. This
allows you to create and destroy elements dynamically and still
detect hover information.
-}
hoverables : a -> { events : Signal a,
                    hoverable : (Bool -> a) -> Element -> Element }
hoverables = Native.Graphics.Input.hoverables

{-| Detect when the mouse is hovering over a specific `Element`. -}
hoverable : Element -> (Element, Signal Bool)
hoverable elem =
    let pool = hoverables False
    in  (pool.hoverable id elem, pool.events)

{-| Represents the current state of a text field. The `string` represents the
characters filling the text field. The `selectionStart` and `selectionEnd`
values represent what the user has selected with their mouse or keyboard.
For example:

        { string="She sells sea shells", selectionStart=3, selectionEnd=0 }

This means the user highlighted the substring `"She"` backwards.
-}
type FieldState = { string:String, selectionStart:Int, selectionEnd:Int }

{-| Create a group of text input fields.

 * The first argument is the default value of the `events` signal.
 * The `events` signal represents all of the activity in this group
   of text fields.
 * The `field` function creates a
   field with the given ghost text and initial field state.
   When the field is modified, the `(FieldState -> a)` function
   takes the new state and turns
   it into a value that can be sent to `events`. For example, this
   lets you add an ID to distinguish between input fields.
-}
fields : a -> { events : Signal a,
                field : (FieldState -> a) -> String -> FieldState -> Element }
fields = Native.Graphics.Input.fields

{-| The empty field state:

        { string="", selectionStart=0, selectionEnd=0 }
-}
emptyFieldState : FieldState
emptyFieldState = { string="", selectionStart=0, selectionEnd=0 }

{-| Create a field with the given default text. The output is an element
that updates to match the user input and a signal of strings representing
the content of the field.
-}
field : String -> (Signal Element, Signal String)
field placeHolder =
    let tfs = fields emptyFieldState
        changes = dropRepeats tfs.events
    in  (lift (tfs.field id placeHolder) changes,
         dropRepeats (lift .string changes))

{-| Same as fields but multiline.
-}
fieldsMultiline : a -> { events : Signal a,
                    field : (FieldState -> a) -> String -> FieldState -> Element }
fieldsMultiline = Native.Graphics.Input.textareas

fieldMultiline : String -> (Signal Element, Signal String)
fieldMultiline placeHolder =
    let tfs = fieldsMultiline emptyFieldState
        changes = dropRepeats tfs.events
    in  (lift (tfs.field id placeHolder) changes,
         dropRepeats (lift .string changes))


{-| Same as `field` but the UI element blocks out each characters. -}
password : String -> (Signal Element, Signal String)
password placeHolder =
    let tfs = Native.Graphics.Input.passwords emptyFieldState
        changes = dropRepeats tfs.events
    in  (lift (tfs.field id placeHolder) changes,
         dropRepeats (lift .string changes))

{-| Same as `field` but it adds an annotation that this field is for email
addresses. This is helpful for auto-complete and for mobile users who may
get a custom keyboard with an `@` and `.com` button.
-}
email : String -> (Signal Element, Signal String)
email placeHolder =
    let tfs = Native.Graphics.Input.emails emptyFieldState
        changes = dropRepeats tfs.events
    in  (lift (tfs.field id placeHolder) changes,
         dropRepeats (lift .string changes))

{-| Create a drop-down menu. When the user selects a string,
the current state of the drop-down is set to the associated
value. This lets you avoid manually mapping the string onto
functions and values.
-}
dropDown : [(String,a)] -> (Signal Element, Signal a)
dropDown = Native.Graphics.Input.dropDown

{-| Create a drop-down menu for selecting strings. The resulting
signal of strings represents the string that is currently selected.
-}
stringDropDown : [String] -> (Signal Element, Signal String)
stringDropDown strs =
    dropDown (List.map (\s -> (s,s)) strs)