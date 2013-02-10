
module Graphics.Color (rgba,rgb,hsva,hsv,
                       complement,
                       red,lime,blue,
                       yellow,cyan,magenta,
                       maroon,navy,green,
                       teal,purple,
                       violet,forestGreen,
                       black,white,gray,grey) where

import Native.Color as Native

data Color = Color Int Int Int Float

rgba = Color
rgb r g b = Color r g b 1

red     = Color 255  0   0  1
lime    = Color  0  255  0  1
blue    = Color  0   0  255 1

yellow  = Color 255 255  0  1
cyan    = Color  0  255 255 1
magenta = Color 255  0  255 1

black   = Color  0   0   0  1
white   = Color 255 255 255 1

gray    = Color 128 128 128 1
grey    = Color 128 128 128 1

maroon  = Color 128  0   0  1
navy    = Color  0   0  128 1
green   = Color  0  128  0  1

teal    = Color  0  128 128 1
purple  = Color 128  0  128 1

forestGreen = Color 34 139 34 1
violet = Color 238 130 238 1

complement = Native.complement
hsva = Native.hsva
hsv = Native.hsv