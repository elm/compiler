
module Mouse where

import Signal (Signal)
import Native.Mouse as Native

-- The current mouse position.
position : Signal (Int,Int)
position = Native.position

-- The current x-coordinate of the mouse.
x : Signal Int
x = Native.x

-- The current y-coordinate of the mouse.
y : Signal Int
y = Native.y

-- The current state of the left mouse-button.
-- True when the button is down, and false otherwise.
isDown : Signal Bool
isDown = Native.isDown

-- True immediately after the left mouse-button has been clicked,
-- and false otherwise.
isClicked : Signal Bool
isClicked = Native.isClicked

-- Always equal to unit. Event triggers on every mouse click.
clicks : Signal ()
clicks = Native.clicks