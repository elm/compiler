-- These are nicely curated inputs from the keyboard. See the
-- [Keyboard.Raw library](/docs/Signal/KeyboardRaw.elm) for a
-- lower-level interface that will let you define more complicated behavior.

module Keyboard where

-- A signal of records indicating which arrow keys are pressed.
--
-- `{ x = 0, y = 0 }` when pressing no arrows.
-- `{ x =-1, y = 0 }` when pressing the left arrow.
-- `{ x = 1, y = 1 }` when pressing the up and right arrows.
-- `{ x = 0, y =-1 }` when pressing the down, left, and right arrows.
arrows : Signal { x:Int, y:Int }
arrows = Native.Keyboard.dir 37 39 38 40

-- Just like the arrows signal, but this uses keys w, a, s, and d,
-- which are common controls for many computer games.
wasd : Signal { x:Int, y:Int }
wasd = Native.Keyboard.dir 65 68 87 83



isDown : KeyCode -> Signal Bool
isDown = Native.Keyboard.isDown

-- Whether the shift key is pressed.
shift : Signal Bool
shift = Native.Keyboard.isDown 16

-- Whether the control key is pressed.
ctrl : Signal Bool
ctrl = Native.Keyboard.isDown 17

-- Whether the space key is pressed.
space : Signal Bool
space = Native.Keyboard.isDown 32
