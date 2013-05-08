
module Graphics.Collage where

import List
import Native.Utils (toFloat)
import Either
import Native.Matrix2D as Matrix
import Native.Graphics.Collage as N
import Graphics.Element
import Color as Color

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

defaultLine : LineStyle
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
solid : Color -> LineStyle
solid  clr = { defaultLine | color <- clr }
dashed : Color -> LineStyle
dashed clr = { defaultLine | color <- clr, dashing <- [8,4] }
dotted : Color -> LineStyle
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
scale s f = { f | scale <- f.scale * s }

move : Float -> Float -> Form -> Form
move x y f = { f | x <- f.x + x, y <- f.y + y }

moveX : Float -> Form -> Form
moveX x f = { f | x <- f.x + x }

moveY : Float -> Form -> Form
moveY y f = { f | y <- f.y + y }

collage : Int -> Int -> [Form] -> Element


type Path = [(Float,Float)]

path : [(Number a,Number a)] -> Path
path ps = ps

segment : (Number a,Number a) -> (Number a,Number a) -> Path
segment p1 p2 = [p1,p2]

type Shape = [(Float,Float)]

polygon : [(Number a,Number a)] -> Shape
polygon points = points

rect : Number a -> Number a -> Shape
rect w h = [ (0-w/2,0-h/2), (0-w/2,h/2), (w/2,h/2), (w/2,0-h/2) ]

oval : Number a -> Number a -> Shape
oval w h =
  let n = 50
      t = 2 * Math.PI / n
      hw = w/2
      hh = h/2
      f i = (hw * Math.cos (t*i), hh * Math.sin (t*i))
  in  map f [0..n-1]

circle : Number a -> Shape
circle r = oval (2*r) (2*r)

ngon : Int -> Number a -> Shape
ngon n r =
  let m = toFloat n
      t = 2 * Math.PI / m
      f i = ( r * Math.cos (t*i), r * Math.sin (t*i) )
  in  map f [0..n-1]
