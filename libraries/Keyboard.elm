module Keyboard where

import Signal (Signal)
import Native.Keyboard

-- Type alias to make it clearer what integers are supposed to represent
-- in this library. Use [`Char.toCode`](docs/Char.elm#toCode) and
-- [`Char.fromCode`](/docs/Char.elm#fromCode) to convert key codes to characters.
type KeyCode = Int

-- Custom key directions so that you can support different locales.
-- The plan is to have a locale independent version of this function
-- that uses the physical location of keys, but I don't know how to do it.
directions : KeyCode -> KeyCode -> KeyCode -> KeyCode -> Signal { x:Int, y:Int }
directions = Native.Keyboard.directions

-- A signal of records indicating which arrow keys are pressed.
--
-- `{ x = 0, y = 0 }` when pressing no arrows.<br>
-- `{ x =-1, y = 0 }` when pressing the left arrow.<br>
-- `{ x = 1, y = 1 }` when pressing the up and right arrows.<br>
-- `{ x = 0, y =-1 }` when pressing the down, left, and right arrows.
arrows : Signal { x:Int, y:Int }
arrows = directions 38 40 37 39

-- Just like the arrows signal, but this uses keys w, a, s, and d,
-- which are common controls for many computer games.
wasd : Signal { x:Int, y:Int }
wasd = directions 87 83 65 68

-- Whether an arbitrary key is pressed.
isDown : KeyCode -> Signal Bool
isDown = Native.Keyboard.isDown

-- Whether the shift key is pressed.
shift : Signal Bool
shift = isDown 16

-- Whether the control key is pressed.
ctrl : Signal Bool
ctrl = isDown 17

-- Whether the space key is pressed.
space : Signal Bool
space = isDown 32

-- Whether the enter key is pressed.
enter : Signal Bool
enter = isDown 13

-- List of keys that are currently down.
keysDown : Signal [KeyCode]
keysDown = Native.Keyboard.keysDown

-- The latest key that has been pressed.
lastPressed : Signal KeyCode
lastPressed = Native.Keyboard.lastPressed