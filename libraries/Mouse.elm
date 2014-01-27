
module Mouse where

{-| Library for working with mouse input.

# Position
@docs position, x, y

# Button Status
@docs isDown, clicks

-}

import Signal (Signal)
import Native.Mouse

{-| The current mouse position. -}
position : Signal (Int,Int)
position = Native.Mouse.position

{-| The current x-coordinate of the mouse. -}
x : Signal Int
x = Native.Mouse.x

{-| The current y-coordinate of the mouse. -}
y : Signal Int
y = Native.Mouse.y

{-| The current state of the left mouse-button.
True when the button is down, and false otherwise. -}
isDown : Signal Bool
isDown = Native.Mouse.isDown

{-| Always equal to unit. Event triggers on every mouse click. -}
clicks : Signal ()
clicks = Native.Mouse.clicks
