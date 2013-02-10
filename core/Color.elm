
module Graphics.Color
    (rgba,rgb
    ,red,green,blue
    ,yellow,cyan,magenta
    ,black,white,gray,grey) where

data Color = Color Int Int Int Float

rgba = Color
rgb r g b = Color r g b 1

red     = Color 255  0   0  1
green   = Color  0  255  0  1
blue    = Color  0   0  255 1
yellow  = Color 255 255  0  1
cyan    = Color  0  255 255 1
magenta = Color 255  0  255 1
black   = Color  0   0   0  1
white   = Color 255 255 255 1
gray    = Color 128 128 128 1
grey    = Color 128 128 128 1
