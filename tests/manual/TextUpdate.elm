import open Graphics.Input

data Input = UserInput String | TickInput Time

stepState: Input -> String -> String
stepState input state =
  case input of
    UserInput s -> state ++ s
    TickInput t -> state

(buttons, buttonSignals) =
  let (plusBtnEl, plusBtnSignal) = button "+"
      plusBtnInput = UserInput <~ ((\x -> "P") <~ plusBtnSignal)
  in  (plusBtnEl, plusBtnInput)

render state = (asText state) `above` buttons

ticker = TickInput <~ fps 25
inputSignal = merge buttonSignals ticker
main = render <~ foldp stepState "" inputSignal