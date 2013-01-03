
module Map where

import JavaScript.Experimental

foreign import jsevent "provideMap"
  (castElementToJSElement $ spacer 640 360)
     jsMaps :: Signal JSElement


center (w,h) elem = container w h middle elem
maps = lift (castJSElementToElement 640 360) jsMaps

main = lift2 center Window.dimensions maps