-- This is an early version of the touch library. It will likely grow to
-- include gestures that would be useful for both games and web-pages.
module Touch where

import Native.Touch as T

type Touch = { x:Int, y:Int, id:Int, x0:Int, y0:Int, t0:Time }

-- A list of touches. Each ongoing touch is represented by a set of
-- coordinates and an identifier id that allows you to distinguish
-- between different touches. Each touch also contains the coordinates and
-- time of the initial contact (x0, y0, and t0) which helps compute more
-- complicated gestures.
touches : Signal [Touch]

-- The last position that was tapped. Default value is `{x=0,y=0}`.
-- Updates whenever the user taps the screen.
taps : Signal { x:Int, y:Int }
