module Color where

{-| Library for working with colors. Includes
[RGB](https://en.wikipedia.org/wiki/RGB_color_model) and
[HSL](http://en.wikipedia.org/wiki/HSL_and_HSV) creation, gradients, and
built-in names.

# Creation
@docs rgb, rgba, hsl, hsla, greyscale, grayscale, complement

# Gradients
@docs linear, radial

# Extracting Colors
@docs toRgb, toHsl

# Built-in Colors
These colors come from the [Tango
palette](http://tango.freedesktop.org/Tango_Icon_Theme_Guidelines)
which provides aesthetically reasonable defaults for colors. Each color also
comes with a light and dark version.

### Standard
@docs red, orange, yellow, green, blue, purple, brown

### Light
@docs lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown

### Dark
@docs darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown

### Eight Shades of Grey
These colors are a compatible series of shades of grey, fitting nicely
with the Tango palette.
@docs white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black

These are identical to the *grey* versions. It seems the spelling is regional, but
that has never helped me remember which one I should be writing.
@docs lightGray, gray, darkGray

-}

import Basics (..)

data Color
    = RGBA Int Int Int Float
    | HSLA Float Float Float Float

{-| Create RGB colors with an alpha component for transparency.
The alpha component is specified with numbers between 0 and 1. -}
rgba : Int -> Int -> Int -> Float -> Color
rgba = RGBA

{-| Create RGB colors from numbers between 0 and 255 inclusive. -}
rgb : Int -> Int -> Int -> Color
rgb r g b = RGBA r g b 1

{-| Create [HSL colors](http://en.wikipedia.org/wiki/HSL_and_HSV)
with an alpha component for transparency.
-}
hsla : Float -> Float -> Float -> Float -> Color
hsla hue saturation lightness alpha =
    HSLA (hue - turns (toFloat (floor (hue / (2*pi))))) saturation lightness alpha

{-| Create [HSL colors](http://en.wikipedia.org/wiki/HSL_and_HSV). This gives
you access to colors more like a color wheel, where all hues are aranged in a
circle that you specify with standard Elm angles (radians).

      red   = hsl (degrees   0) 1 0.5
      green = hsl (degrees 120) 1 0.5
      blue  = hsl (degrees 240) 1 0.5

      pastelRed = hsl (degrees 0) 0.7 0.7

To cycle through all colors, just cycle through degrees. The saturation level
is how vibrant the color is, like a dial between grey and bright colors. The
lightness level is a dial between white and black.
-}
hsl : Float -> Float -> Float -> Color
hsl hue saturation lightness =
    hsla hue saturation lightness 1

{-| Produce a gray based on the input. 0 is white, 1 is black. -}
grayscale : Float -> Color
grayscale p = HSLA 0 0 (1-p) 1

greyscale : Float -> Color
greyscale p = HSLA 0 0 (1-p) 1

{-| Produce a &ldquo;complementary color&rdquo;. The two colors will
accent each other. This is the same as rotating the hue by 180&deg;.
-}
complement : Color -> Color
complement color =
    case color of
      HSLA h s l a -> hsla (h + degrees 180) s l a
      RGBA r g b a -> let (h,s,l) = rgbToHsl r g b
                      in  hsla (h + degrees 180) s l a

{-| Extract the components of a color in the HSL format.
-}
toHsl : Color -> { hue:Float, saturation:Float, lightness:Float, alpha:Float }
toHsl color =
    case color of
      HSLA h s l a -> { hue=h, saturation=s, lightness=l, alpha=a }
      RGBA r g b a ->
          let (h,s,l) = rgbToHsl r g b
          in  { hue=h, saturation=s, lightness=l, alpha=a }

{-| Extract the components of a color in the RGB format.
-}
toRgb : Color -> { red:Int, green:Int, blue:Int, alpha:Float }
toRgb color =
    case color of
      RGBA r g b a -> { red=r, green=g, blue=b, alpha=a }
      HSLA h s l a ->
          let (r,g,b) = hslToRgb h s l
          in  { red   = round (255 * r)
              , green = round (255 * g)
              , blue  = round (255 * b)
              , alpha = a
              }

fmod : Float -> Int -> Float
fmod f n =
  let integer = floor f
  in  toFloat (integer % n) + f - toFloat integer

rgbToHsl : Int -> Int -> Int -> (Float,Float,Float)
rgbToHsl red green blue =
  let r = toFloat red   / 255
      g = toFloat green / 255
      b = toFloat blue  / 255

      cMax = max (max r g) b
      cMin = min (min r g) b

      c = cMax - cMin

      hue = degrees 60 * if | cMax == r -> ((g - b) / c) `fmod` 6
                            | cMax == g -> ((b - r) / c) + 2
                            | cMax == b -> ((r - g) / c) + 4

      lightness = (cMax + cMin) / 2

      saturation =
          if lightness == 0
            then 0
            else c / (1 - abs (2 * lightness - 1))
  in
      (hue, saturation, lightness)

hslToRgb : Float -> Float -> Float -> (Float,Float,Float)
hslToRgb hue saturation lightness =
  let chroma = (1 - abs (2 * lightness - 1)) * saturation
      hue' = hue / degrees 60

      x = chroma * (1 - abs (fmod hue' 2 - 1))

      (r,g,b) = if | hue' < 0  -> (0, 0, 0)
                   | hue' < 1  -> (chroma, x, 0)
                   | hue' < 2  -> (x, chroma, 0)
                   | hue' < 3  -> (0, chroma, x)
                   | hue' < 4  -> (0, x, chroma)
                   | hue' < 5  -> (x, 0, chroma)
                   | hue' < 6  -> (chroma, 0, x)
                   | otherwise -> (0, 0, 0)

      m = lightness - chroma / 2
  in
      (r + m, g + m, b + m)

--toV3 : Color -> V3

--toV4 : Color -> V4

data Gradient
  = Linear (Float,Float) (Float,Float) [(Float,Color)]
  | Radial (Float,Float) Float (Float,Float) Float [(Float,Color)]

{-| Create a linear gradient. Takes a start and end point and then a series of
&ldquo;color stops&rdquo; that indicate how to interpolate between the start and
end points. See [this example](http://elm-lang.org/edit/examples/Elements/LinearGradient.elm) for a
more visual explanation. -}
linear : (number, number) -> (number, number) -> [(Float,Color)] -> Gradient
linear = Linear

{-| Create a radial gradient. First takes a start point and inner radius.  Then
takes an end point and outer radius. It then takes a series of &ldquo;color
stops&rdquo; that indicate how to interpolate between the inner and outer
circles. See [this example](http://elm-lang.org/edit/examples/Elements/RadialGradient.elm) for a
more visual explanation. -}
radial : (number,number) -> number -> (number,number) -> number -> [(Float,Color)] -> Gradient
radial = Radial


-- BUILT-IN COLORS

lightRed    : Color
lightRed    = RGBA 239 41 41 1
red    : Color
red    = RGBA 204  0  0 1
darkRed    : Color
darkRed    = RGBA 164  0  0 1

lightOrange : Color
lightOrange = RGBA 252 175 62 1
orange : Color
orange = RGBA 245 121  0 1
darkOrange : Color
darkOrange = RGBA 206  92  0 1

lightYellow : Color
lightYellow = RGBA 255 233 79 1
yellow : Color
yellow = RGBA 237 212  0 1
darkYellow : Color
darkYellow = RGBA 196 160  0 1

lightGreen  : Color
lightGreen  = RGBA 138 226  52 1
green  : Color
green  = RGBA 115 210  22 1
darkGreen  : Color
darkGreen  = RGBA  78 154   6 1

lightBlue   : Color
lightBlue   = RGBA 114 159 207 1
blue   : Color
blue   = RGBA  52 101 164 1
darkBlue   : Color
darkBlue   = RGBA  32  74 135 1

lightPurple : Color
lightPurple = RGBA 173 127 168 1
purple : Color
purple = RGBA 117  80 123 1
darkPurple : Color
darkPurple = RGBA  92  53 102 1

lightBrown  : Color
lightBrown  = RGBA 233 185 110 1
brown  : Color
brown  = RGBA 193 125  17 1
darkBrown  : Color
darkBrown  = RGBA 143  89   2 1

black         : Color
black         = RGBA  0   0   0  1
white         : Color
white         = RGBA 255 255 255 1

lightGrey     : Color
lightGrey     = RGBA 238 238 236 1
grey          : Color
grey          = RGBA 211 215 207 1
darkGrey      : Color
darkGrey      = RGBA 186 189 182 1

lightGray : Color
lightGray = RGBA 238 238 236 1
gray      : Color
gray      = RGBA 211 215 207 1
darkGray  : Color
darkGray  = RGBA 186 189 182 1

lightCharcoal : Color
lightCharcoal = RGBA 136 138 133 1
charcoal      : Color
charcoal      = RGBA  85  87  83 1
darkCharcoal  : Color
darkCharcoal  = RGBA  46  52  54 1
