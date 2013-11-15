module Keyboard where

{-| Library for working with keyboard input.

# Representing Keys
@docs KeyCode

# Directions
@docs arrows, wasd, directions

# Specific Keys
@docs shift, enter, space, ctrl

# General Keypresses
@docs isDown, keysDown, lastPressed

-}

import Signal (Signal)
import Native.Keyboard
import Keyboard.Keys

{-| Custom key directions to support different locales. The order is up, down,
left, right.
-}
directions : Keyboard.Keys.KeyCode -> Keyboard.Keys.KeyCode -> Keyboard.Keys.KeyCode -> Keyboard.Keys.KeyCode -> Signal { x:Int, y:Int }
directions = Native.Keyboard.directions

directionKeys: Keyboard.Keys.Key -> Keyboard.Keys.Key -> Keyboard.Keys.Key -> Keyboard.Keys.Key -> Signal { x:Int, y:Int }
directionKeys up down right left = Native.Keyboard.directions up.keyCode down.keyCode right.keyCode left.keyCode

{-| A signal of records indicating which arrow keys are pressed.

`{ x = 0, y = 0 }` when pressing no arrows.<br>
`{ x =-1, y = 0 }` when pressing the left arrow.<br>
`{ x = 1, y = 1 }` when pressing the up and right arrows.<br>
`{ x = 0, y =-1 }` when pressing the down, left, and right arrows.
-}
arrows : Signal { x:Int, y:Int }
arrows = directionKeys Keyboard.Keys.arrowUp Keyboard.Keys.arrowDown Keyboard.Keys.arrowRight Keyboard.Keys.arrowLeft

{-| Just like the arrows signal, but this uses keys w, a, s, and d,
which are common controls for many computer games.
-}
wasd : Signal { x:Int, y:Int }
wasd = directionKeys Keyboard.Keys.w Keyboard.Keys.s Keyboard.Keys.a Keyboard.Keys.d

{-| Whether an arbitrary key is pressed. -}
isDown : Keyboard.Keys.KeyCode -> Signal Bool
isDown = Native.Keyboard.isDown

isKeyDown : Keyboard.Keys.Key -> Signal Bool
isKeyDown k = isDown k.keyCode

{-| Whether the shift key is pressed. -}
shift : Signal Bool
shift = isKeyDown Keyboard.Keys.shift

{-| Whether the control key is pressed. -}
ctrl : Signal Bool
ctrl = isKeyDown Keyboard.Keys.ctrl

{-| Whether the space key is pressed. -}
space : Signal Bool
space = isKeyDown Keyboard.Keys.space

{-| Whether the enter key is pressed. -}
enter : Signal Bool
enter = isKeyDown Keyboard.Keys.enter

{-| List of keys that are currently down. -}
keysDown : Signal [Keyboard.Keys.KeyCode]
keysDown = Native.Keyboard.keysDown

{-| The latest key that has been pressed. -}
lastPressed : Signal Keyboard.Keys.KeyCode
lastPressed = Native.Keyboard.lastPressed