
module Keyboard.Raw where

import Native.Keyboard.Raw

type KeyCode = Int

keysDown : Signal [KeyCode]
charPressed : Signal (Maybe Char)
