
module Color where

import Native.Color

data Color = Color Int Int Int Float

-- Create RGB colors with an alpha component for transparency.
-- The alpha component is specified with numbers between 0 and 1.
rgba : Int -> Int -> Int -> Float -> Color
rgba = Color

-- Create RGB colors from numbers between 0 and 255 inclusive.
rgb : Int -> Int -> Int -> Color
rgb r g b = Color r g b 1

red  : Color
red  = Color 255  0   0  1
lime : Color
lime = Color  0  255  0  1
blue : Color
blue = Color  0   0  255 1

yellow  : Color
yellow  = Color 255 255  0  1
cyan    : Color
cyan    = Color  0  255 255 1
magenta : Color
magenta = Color 255  0  255 1

black : Color
black = Color  0   0   0  1
white : Color
white = Color 255 255 255 1

gray : Color
gray = Color 128 128 128 1
grey : Color
grey = Color 128 128 128 1

maroon : Color
maroon = Color 128  0   0  1
navy   : Color
navy   = Color  0   0  128 1
green  : Color
green  = Color  0  128  0  1

teal   : Color
teal   = Color  0  128 128 1
purple : Color
purple = Color 128  0  128 1

violet : Color
violet = Color 238 130 238 1
forestGreen : Color
forestGreen = Color 34 139 34 1

-- Produce a &ldquo;complementary color&rdquo;.
-- The two colors will accent each other.
complement : Color -> Color
complement = Native.Color.complement

-- Create [HSV colors](http://en.wikipedia.org/wiki/HSL_and_HSV)
-- with an alpha component for transparency.
hsva : Float -> Float -> Float -> Float -> Color
hsva = Native.Color.hsva

-- Create [HSV colors](http://en.wikipedia.org/wiki/HSL_and_HSV).
-- This is very convenient for creating colors that cycle and shift.
--
--         hsv (degrees 240) 1 1 == blue
hsv : Float -> Float -> Float -> Color
hsv = Native.Color.hsv

data Gradient
  = Linear (Float,Float) (Float,Float) [(Float,Color)]
  | Radial (Float,Float) Float (Float,Float) Float [(Float,Color)]

-- Create a linear gradient. Takes a start and end point and then a series
-- of &ldquo;color stops&rdquo; that indicate how to interpolate between
-- the start and end points. See [this example](/edit/examples/Elements/LinearGradient.elm) for
-- a more visual explanation.
linear : (number, number) -> (number, number) -> [(Float,Color)] -> Gradient
linear = Linear

-- Create a radial gradient. First takes a start point and inner radius.
-- Then takes an end point and outer radius. It then takes a series
-- of &ldquo;color stops&rdquo; that indicate how to interpolate between
-- the inner and outer circles. See [this example](/edit/examples/Elements/RadialGradient.elm) for
-- a more visual explanation.
radial : (number,number) -> number -> (number,number) -> number -> [(Float,Color)] -> Gradient
radial = Radial
