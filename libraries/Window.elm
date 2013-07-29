
module Window where

import Signal (Signal)
import Native.Window

-- The current dimensions of the window (i.e. the area viewable to the
-- user, not including scroll bars).
dimensions : Signal (Int,Int)
dimensions = Native.Window.dimensions

-- The current width of the window.
width : Signal Int
width = Native.Window.width

-- The current height of the window.
height : Signal Int
height = Native.Window.height
