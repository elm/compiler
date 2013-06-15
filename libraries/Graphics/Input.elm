
module Graphics.Input where

import Signal (lift,dropRepeats)
import Native.Graphics.Input as N
import List

id x = x

-- Create a group of buttons.
--
-- * The first argument is the default value of the `events` signal.
-- * The `events` signal represents all of the activity in this group
--   of buttons.
-- * The `button` function creates a button
--   with the given name, like &ldquo;Submit&rdquo; or &ldquo;Cancel&rdquo;.
--   The `a` value is sent to `events` whenever the button is pressed.
buttons : a -> { events : Signal a,
                 button : a -> String -> Element }

-- Create a button with a given label. The result is an `Element` and
-- a signal of units. This signal triggers whenever the button is pressed.
button : String -> (Element, Signal ())
button txt =
    let pool = N.buttons ()
    in  (pool.button () txt, pool.events)

-- Create a group of custom buttons.
--
-- * The first argument is the default value of the `events` signal.
-- * The `events` signal represents all of the activity in this group
--   of custom buttons.
-- * The `customButton` function creates a button with three different visual
--   states, one for up, hovering, and down. The resulting button has dimensions
--   large enough to fit all three possible `Elements`.
--   The `a` value is sent to `events` whenever the button is pressed.
customButtons : a -> { events : Signal a,
                       customButton : a -> Element -> Element -> Element -> Element }

-- Create a button with custom states for up, hovering, and down
-- (given in that order). The result is an `Element` and
-- a signal of units. This signal triggers whenever the button is pressed.
customButton : Element -> Element -> Element -> (Element, Signal ())
customButton up hover down =
    let pool = N.customButtons ()
    in  (pool.customButton () up hover down, pool.events)

-- Create a group of checkboxes.
--
-- * The first argument is the default value of the `events` signal.
-- * The `events` signal represents all of the activity in this group
--   of checkboxes.
-- * The `checkbox` function creates a
--   checkbox with a given state. The `(Bool -> a)` function is used
--   when the checkbox is modified. It takes the new state and turns
--   it into a value that can be sent to `events`. For example, this
--   lets you add an ID to distinguish between checkboxes.
checkboxes : a -> { events : Signal a,
                    checkbox : (Bool -> a) -> Bool -> Element }

-- Create a checkbox with a given start state. Unlike `button`, this result
-- is a *signal* of elements. That is because a checkbox has state that
-- updates based on user input.
-- The boolean signal represents the current state of the checkbox.
checkbox : Bool -> (Signal Element, Signal Bool)
checkbox b =
    let cbs = N.checkboxes b
    in  (lift (cbs.box id) cbs.events, cbs.events)

hoverables : a -> { events : Signal a,
                    hoverable : (Bool -> a) -> Element -> Element }

hoverable : Element -> (Element, Signal Bool)
hoverable elem =
    let pool = N.hoverables False
    in  (pool.hoverable id elem, pool.events)

-- Represents the current state of a text field. The `string` represents the
-- characters filling the text field. The `selectionStart` and `selectionEnd`
-- values represent what the user has selected with their mouse or keyboard.
-- For example:
--
--         { string="She sells sea shells", selectionStart=3, selectionEnd=0 }
--
-- This means the user highlighted the substring `"She"` backwards.
type FieldState = { string:String, selectionStart:Int, selectionEnd:Int }

-- Create a group of text input fields.
--
-- * The first argument is the default value of the `events` signal.
-- * The `events` signal represents all of the activity in this group
--   of text fields.
-- * The `field` function creates a
--   field with the given ghost text and initial field state.
--   When the field is modified, the `(FieldState -> a)` function
--   takes the new state and turns
--   it into a value that can be sent to `events`. For example, this
--   lets you add an ID to distinguish between input fields.
fields : a -> { events : Signal a,
                field : (FieldState -> a) -> String -> FieldState -> Element }

-- The empty field state:
--
--         { string="", selectionStart=0, selectionEnd=0 }
emptyFieldState : FieldState
emptyFieldState = { string="", selectionStart=0, selectionEnd=0 }

-- Create a field with the given default text. The output is an element that
-- updates to match the user input and a signal of strings representing the
-- content of the field.
field : String -> (Signal Element, Signal String)
field placeHolder =
    let tfs = N.fields emptyFieldState
        changes = dropRepeats tfs.events
    in  (lift (tfs.field id placeHolder) changes,
         dropRepeats (lift .string changes))

-- Same as `field` but the UI element blocks out each characters.
password : String -> (Signal Element, Signal String)
password placeHolder =
    let tfs = N.passwords emptyFieldState
        changes = dropRepeats tfs.events
    in  (lift (tfs.field id placeHolder) changes,
         dropRepeats (lift .string changes))

-- Same as `field` but it adds an annotation that this field is for email
-- addresses. This is helpful for auto-complete and for mobile users who may
-- get a custom keyboard with an `@` and `.com` button.
email : String -> (Signal Element, Signal String)
email placeHolder =
    let tfs = N.emails emptyFieldState
        changes = dropRepeats tfs.events
    in  (lift (tfs.field id placeHolder) changes,
         dropRepeats (lift .string changes))

-- Create a drop-down menu. When the user selects a string,
-- the current state of the drop-down is set to the associated
-- value. This lets you avoid manually mapping the string onto
-- functions and values.
dropDown : [(String,a)] -> (Signal Element, Signal a)

-- Create a drop-down menu for selecting strings. The resulting
-- signal of strings represents the string that is currently selected.
stringDropDown : [String] -> (Signal Element, Signal String)
stringDropDown strs =
    N.dropDown (List.map (\s -> (s,s)) strs)