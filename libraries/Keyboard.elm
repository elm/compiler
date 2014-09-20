module Keyboard where
{-| Library for working with keyboard input.

# Representing Keys
@docs KeyCode

# Directions
@docs arrows, wasd, directions

# Specific Keys
The following signals are `True` when the particular key is pressed and `False`
otherwise.

@docs enter, space, ctrl, shift, alt, meta

# General Keypresses
@docs isDown, keysDown, lastPressed

-}

import Signal (Signal)
import Native.Keyboard

{-| Type alias to make it clearer what integers are supposed to represent
in this library. Use `Char.toCode` and `Char.fromCode` to convert key codes
to characters. Use the uppercase character with `toCode`.
-}
type alias KeyCode = Int

{-| Custom key directions to support different locales. The order is up, down,
left, right.
-}
directions : KeyCode -> KeyCode -> KeyCode -> KeyCode -> Signal { x:Int, y:Int }
directions = Native.Keyboard.directions

{-| A signal of records indicating which arrow keys are pressed.

`{ x = 0, y = 0 }` when pressing no arrows.<br>
`{ x =-1, y = 0 }` when pressing the left arrow.<br>
`{ x = 1, y = 1 }` when pressing the up and right arrows.<br>
`{ x = 0, y =-1 }` when pressing the down, left, and right arrows.
-}
arrows : Signal { x:Int, y:Int }
arrows = directions 38 40 37 39

{-| Just like the arrows signal, but this uses keys w, a, s, and d,
which are common controls for many computer games.
-}
wasd : Signal { x:Int, y:Int }
wasd = directions 87 83 65 68

{-| Whether an arbitrary key is pressed. -}
isDown : KeyCode -> Signal Bool
isDown = Native.Keyboard.isDown

alt   : Signal Bool
alt   = Native.Keyboard.alt

ctrl  : Signal Bool
ctrl  = isDown 17

{-| The meta key is the Windows key on Windows and the Command key on Mac.
-}
meta  : Signal Bool
meta  = Native.Keyboard.meta

shift : Signal Bool
shift = isDown 16

space : Signal Bool
space = isDown 32

enter : Signal Bool
enter = isDown 13

{-| List of keys that are currently down. -}
keysDown : Signal [KeyCode]
keysDown = Native.Keyboard.keysDown

{-| The latest key that has been pressed. -}
lastPressed : Signal KeyCode
lastPressed = Native.Keyboard.lastPressed
