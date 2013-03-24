
module Graphics.Input where

buttons : a -> { button : a -> String -> Element
               , events : Signal a
               }

button : String -> (Element, Signal ())
button txt =
    let pool = buttons ()
    in  (pool.button () txt, pool.events)

customButtons : a -> { button : a -> Element -> Element -> Element -> Element
                     , events : Signal a
                     }

customButton : Element -> Element -> Element -> (Element, Signal ())
customButton up hover down =
    let pool = customButtons ()
    in  (pool.button () up hover down, pool.events)

checkBoxes : a -> { box : (Bool -> a) -> Bool -> Element
                  , events : Signal a
                  }

checkBox : Bool -> (Signal Element, Signal Bool)
checkBox b =
    let cbs = checkBoxes b
    in  (lift (cbs.box id) cbs.events, cbs.events)