
module Mouse where

import Native.Mouse

-- The current mouse position.
position : Signal (Int,Int)

-- The current x-coordinate of the mouse.
x : Signal Int

-- The current y-coordinate of the mouse.
y : Signal Int


-- The current state of the left mouse-button.
-- True when the button is down, and false otherwise.
isDown : Signal Bool

-- True immediately after the left mouse-button has been clicked,
-- and false otherwise.
isClicked : Signal Bool
 

-- Always equal to unit. Event triggers on every mouse click.
clicks : Signal ()
