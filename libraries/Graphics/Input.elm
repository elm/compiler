
module Graphics.Input where

import Signal (lift)
import Native.Graphics.Input as N

id x = x

buttons : a -> { button : a -> String -> Element, events : Signal a }

button : String -> (Element, Signal ())
button txt =
    let pool = N.buttons ()
    in  (pool.button () txt, pool.events)

customButtons : a -> { button : a -> Element -> Element -> Element -> Element,
                       events : Signal a }

customButton : Element -> Element -> Element -> (Element, Signal ())
customButton up hover down =
    let pool = N.customButtons ()
    in  (pool.button () up hover down, pool.events)

checkboxes : a -> { box : (Bool -> a) -> Bool -> Element, events : Signal a }

checkbox : Bool -> (Signal Element, Signal Bool)
checkbox b =
    let cbs = N.checkboxes b
    in  (lift (cbs.box id) cbs.events, cbs.events)

type FieldState = { string:String, start:Int, end:Int }

fields : a -> { field : (FieldState -> a) -> String -> FieldState -> Element,
                events : Signal a }

empty = { string="", start=0, end=0 }

field : String -> (Signal Element, Signal FieldState)
field placeHolder =
    let tfs = N.fields empty
    in  (lift (tfs.field id placeHolder) tfs.events, tfs.events)

password : String -> (Signal Element, Signal FieldState)
password placeHolder =
    let tfs = N.passwords empty
    in  (lift (tfs.field id placeHolder) tfs.events, tfs.events)

email : String -> (Signal Element, Signal FieldState)
email placeHolder =
    let tfs = N.emails empty
    in  (lift (tfs.field id placeHolder) tfs.events, tfs.events)

-- file?