-- These are nicely curated inputs from the keyboard. See the
-- [Keyboard.Raw library](/docs/Signal/KeyboardRaw.elm) for a
-- lower-level interface that will let you define more complicated behavior.
module Keyboard where

import Native.Keyboard as N

type KeyCode = Int

-- A signal of records indicating which arrow keys are pressed.
--
-- `{ x = 0, y = 0 }` when pressing no arrows.
-- `{ x =-1, y = 0 }` when pressing the left arrow.
-- `{ x = 1, y = 1 }` when pressing the up and right arrows.
-- `{ x = 0, y =-1 }` when pressing the down, left, and right arrows.
arrows : Signal { x:Int, y:Int }
arrows = N.directions 38 40 37 39

-- Just like the arrows signal, but this uses keys w, a, s, and d,
-- which are common controls for many computer games.
wasd : Signal { x:Int, y:Int }
wasd = N.directions 87 83 65 68

-- Custom key directions so that you can support different locales.
-- The plan is to have a locale independent version of this function
-- that uses the physical location of keys, but I don't know how to do it.
-- left, right, up, down
directions : KeyCode -> KeyCode -> KeyCode -> KeyCode -> Signal { x:Int, y:Int }

-- Whether an arbitrary key is pressed.
isDown : KeyCode -> Signal Bool

-- Whether the shift key is pressed.
shift : Signal Bool
shift = N.isDown 16

-- Whether the control key is pressed.
ctrl : Signal Bool
ctrl = N.isDown 17

-- Whether the space key is pressed.
space : Signal Bool
space = N.isDown 32

-- Whether the enter key is pressed.
enter : Signal Bool
enter = N.isDown 13

-- List of keys that are currently down.
keysDown : Signal [KeyCode]

-- The latest key that has been pressed.
lastPressed : Signal Char
