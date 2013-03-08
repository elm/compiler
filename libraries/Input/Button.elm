
module Input.Button where

type ButtonPool t = { button : String -> t -> Element, presses : Signal t }

buttonPool : String -> ButtonPool t