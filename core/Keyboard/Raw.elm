
module Keyboard.Raw where

import Native.Keyboard.Raw

type KeyCode = Int

keysDown : Signal [KeyCode]
keysDown = Native.Keyboard.Raw.keysDown

charPressed : Signal (Maybe Char)
charPressed = Native.Keyboard.Raw.charPressed