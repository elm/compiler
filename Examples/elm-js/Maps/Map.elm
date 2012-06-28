
module Map where

import Signal.Window as Win
import Foreign.JavaScript

foreign import jsevent "provideMap"
  (castElementToJSElement $ rectangle 640 360)
     jsMaps :: Signal JSElement


center (w,h) elem = size w h (box 5 elem)
maps = lift (castJSElementToElement 640 360) jsMaps

main = lift2 center Win.dimensions maps