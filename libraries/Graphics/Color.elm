
module Graphics.Color (rgba,rgb,hsva,hsv,
                       complement,
                       red,lime,blue,
                       yellow,cyan,magenta,
                       maroon,navy,green,
                       teal,purple,
                       violet,forestGreen,
                       black,white,gray,grey,
                       linear,radial) where

data Color = Color Int Int Int Float

-- Create RGB colors with an alpha component for transparency. The alpha component is specified with numbers between 0 and 1.
rgba = Color

-- Create RGB colors from numbers between 0 and 255 inclusive.
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

-- Produce a &ldquo;complementary color&rdquo;. The two colors will accent each other.
complement : Color -> Color

-- Create HSV colors with an alpha component for transparency. The alpha component is specified with numbers between 0 and 1.
hsva : Int -> Float -> Float -> Float -> Color

-- Create HSV colors. HSV stands for hue-saturation-value.
--
-- Hue is a degree from 0 to 360 representing a color wheel: red at 0&deg;, green at 120&deg;, blue at 240&deg;, and red again at 360&deg;.
-- This makes it easy to cycle through colors and compute color complements, triads, tetrads, etc.
--
-- Saturation is a number between 1 and 0 where lowering this number makes your color more grey. This can help you tone a color down.
--
-- Value is also a number between 1 and 0. Lowering this number makes your color more black.
--
-- Look up the &ldquo;HSV cylinder&rdquo; for more information.
hsv : Int -> Float -> Float -> Color

data Gradient
  = Linear [(Float,Color)] (Float,Float) (Float,Float)
  | Radial [(Float,Color)] (Float,Float) Float (Float,Float) Float

linear = Linear
radial = Radial
