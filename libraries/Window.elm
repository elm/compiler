
module Window where

import Native.Window as W

-- The current dimensions of the window (i.e. the area viewable to the
-- user, not including scroll bars).
dimensions : Signal (Int,Int)

-- The current width of the window.
width : Signal Int

-- The current height of the window.
height : Signal Int
