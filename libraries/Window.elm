
module Window where

{-| Provides information about the container that your Elm program lives in.
When you embed Elm in a `<div>` it gives the dimensions of the container, not
the whole window.

# Dimensions
@docs dimensions, width, height

-}

import Signal (Signal)
import Native.Window

{-| The current width and height of the window (i.e. the area viewable to the
user, not including scroll bars). -}
dimensions : Signal (Float,Float)
dimensions = Native.Window.dimensions

{-| The current width of the window. -}
width : Signal Float
width = Native.Window.width

{-| The current height of the window. -}
height : Signal Float
height = Native.Window.height
