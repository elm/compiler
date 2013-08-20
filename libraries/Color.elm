
module Color where

import Native.Color
import Basics ((-))

data Color = Color Int Int Int Float

-- Create RGB colors with an alpha component for transparency.
-- The alpha component is specified with numbers between 0 and 1.
rgba : Int -> Int -> Int -> Float -> Color
rgba = Color

-- Create RGB colors from numbers between 0 and 255 inclusive.
rgb : Int -> Int -> Int -> Color
rgb r g b = Color r g b 1

lightYellow = Color 255 233 79 1
yellow      = Color 237 212  0 1
darkYellow  = Color 196 160  0 1

lightOrange = Color 252 175 62 1
orange      = Color 245 121  0 1
darkOrange  = Color 206  92  0 1

lightBrown = Color 233 185 110 1
brown      = Color 193 125  17 1
darkBrown  = Color 143  89   2 1

lightGreen = Color 138 226  52 1
green      = Color 115 210  22 1
darkGreen  = Color  78 154   6 1

lightBlue = Color 114 159 207 1
blue      = Color  52 101 164 1
darkBlue  = Color  32  74 135 1

lightPurple = Color 173 127 168 1
purple      = Color 117  80 123 1
darkPurple  = Color  92  53 102 1

lightRed = Color 239 41 41 1
red      = Color 204  0  0 1
darkRed  = Color 164  0  0 1

black = Color  0   0   0  1
white = Color 255 255 255 1

lightGrey = Color 238 238 236
grey      = Color 211 215 207
darkGrey  = Color 186 189 182

lightGray = Color 238 238 236
gray      = Color 211 215 207
darkGray  = Color 186 189 182

lightCharcoal = Color 136 138 133
charcoal      = Color  85  87  83
darkCharcoal  = Color  46  52  54

grayscale : Float -> Color
grayscale p = hsv 0 0 (1-p)
greyscale p = hsv 0 0 (1-p)

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
-- Hue is an angle and should be given in standard Elm angles (radians).
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
