
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

checkBoxes : a -> { box : (Bool -> a) -> Bool -> Element, events : Signal a }

checkBox : Bool -> (Signal Element, Signal Bool)
checkBox b =
    let cbs = N.checkBoxes b
    in  (lift (cbs.box id) cbs.events, cbs.events)

type TextState = { text:String, start:Int, end:Int }

textFields : a -> { field : (TextState -> a) -> String -> TextState -> Element,
                    events : Signal a }

text : String -> TextState -> (Signal Element, Signal TextState)
text placeHolder textState =
    let tfs = N.textFields textState
    in  (lift (tfs.field id placeHolder) tfs.events, tfs.events)

password : String -> TextState -> (Signal Element, Signal TextState)
password placeHolder textState =
    let tfs = N.passwords textState
    in  (lift (tfs.field id placeHolder) tfs.events, tfs.events)

email : String -> TextState -> (Signal Element, Signal TextState)
email placeHolder textState =
    let tfs = N.emails textState
    in  (lift (tfs.field id placeHolder) tfs.events, tfs.events)

-- file?