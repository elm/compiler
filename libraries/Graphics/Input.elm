
module Graphics.Input where

import Native.Graphics.Input as N

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

type TextState = {
  input : String,
  start : Int,
  end   : Int
 }

textFields : a -> { field : (TextState -> a) -> String -> TextState -> Element,
                    events : Signal a }

textField : String -> TextState -> (Signal Element, Signal TextState)
textField placeHolder textState =
    let tfs = N.textFields textState
    in  (lift (tfs.field id placeHolder) tfs.events, tfs.events)

password : String -> TextState -> (Signal Element, Signal TextState)
email : String -> TextState -> (Signal Element, Signal TextState)

-- file?