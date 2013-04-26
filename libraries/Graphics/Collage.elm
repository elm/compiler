
module Graphics.Collage where

import Either
import Graphics.Geometry
import Native.Graphics.Matrix as Matrix
import Native.Graphics.Collage as N
import Graphics.Element
import Graphics.Color as Color

type Form = {
  theta : Float,
  scale : Float,
  x : Float,
  y : Float,
  form : BasicForm
 }

data FillStyle
  = Solid Color
  | Texture String
  | Gradient Gradient

data LineCap  = Butt  | Round | Square
data LineJoin = Smooth | Sharp | Clipped

type LineStyle = {
  color : Color,
  width : Float,
  cap   : LineCap,
  join  : LineJoin,
  miterLimit : Float,
  dashing    : [Int],
  dashOffset : Int
 }

defaultLine = {
  color = Color.black,
  width = 1,
  cap   = Butt,
  join  = Sharp,
  dashing = [],
  dashOffset = 0,
  miterLimit = 10
 }

-- default LineStyles
solid  clr = { defaultLine | color <- clr }
dashed clr = { defaultLine | color <- clr, dashing <- [8,4] }
dotted clr = { defaultLine | color <- clr, dashing <- [3,3] }

data BasicForm
  = FPath LineStyle Path
  | FShape (Either LineStyle FillStyle) Shape
  | FImage Int Int (Int,Int) String
  | FElement Element
  | FGroup Matrix [Form]

form f = { theta = 0, scale = 1, x = 0, y = 0, form = f }

fill style shape = form (FShape (Right style) shape)

filled : Color -> Shape -> Form
filled color shape = fill (Solid color) shape

textured : String -> Shape -> Form
textured src shape = fill (Texture src) shape

gradient : Gradient -> Shape -> Form
gradient grad shape = fill (Gradient grad) shape

outlined : LineStyle -> Shape -> Form
outlined style shape = form (FShape (Left style) shape)

traced : LineStyle -> Path -> Form
traced style path = form (FPath style path)

-- Draw a sprite
-- w width
-- h height
-- pos (px,py) coordinates into the sprite map
-- src The location of the image
sprite : Int -> Int -> (Int,Int) -> String -> Form
sprite w h pos src = form (FImage w h pos src)

toForm : Element -> Form
toForm e = form (FElement e)

group : [Form] -> Form
group fs = form (FGroup Matrix.identity fs)

groupTransform : Matrix -> [Form] -> Form
groupTransform matrix fs = form (FGroup matrix fs)

rotate : Float -> Form -> Form
rotate t f = { f | theta <- f.theta + t }

scale : Float -> Form -> Form
scale  s f = { f | scale <- f.scale * s }

move x y f = { f | x <- f.x + x, y <- f.y + y }
moveX  x f = { f | x <- f.x + x }
moveY  y f = { f | y <- f.y + y }

collage : Int -> Int -> [Form] -> Element
