module Debug where

import Graphics.Element (Element)
import Native.Debug

tracePath : String -> Element -> Element
tracePath = Native.Debug.tracePath