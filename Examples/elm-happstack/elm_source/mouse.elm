
-- Move your mouse around above the canvas!

import Signal.Mouse (position)
import Signal.Window (dimensions)

myBlue  = rgb 0 85 170
myGreen = rgba 28 267 85 (1/2)

scene (x,y) (w,h) =
  collage w h [ filled myBlue . rotate ((x+y)/1000) $ ngon 4 100 (200,200)
              , filled myGreen $ ngon 5 30 (x,y)
              ]

main = lift2 scene position dimensions