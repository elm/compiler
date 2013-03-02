
module Graphics.Text where

import Graphics.Color
import JavaScript

type Style = {
  typeface : JSString,
  href     : JSString,
  height   : Float,
  color    : Color,
  bold     : Bool,
  italic   : Bool,
  overline : Bool,
  underline : Bool,
  strikeThrough : Bool
}