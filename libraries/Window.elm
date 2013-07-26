
module Window where

import Native.Window as Native

-- The current dimensions of the window (i.e. the area viewable to the
-- user, not including scroll bars).
dimensions : Signal (Int,Int)
dimensions = Native.dimensions

-- The current width of the window.
width : Signal Int
width = Native.width

-- The current height of the window.
height : Signal Int
height = Native.height
