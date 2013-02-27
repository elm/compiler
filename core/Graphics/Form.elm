
module Collage where

data Path = Path [(Float,Float)]

path = Path
segment p1 p2 = Path [p1,p2]

data Shape = Shape [(Float,Float)]

polygon = Shape

rect w h = polygon [ (0-w/2,0-h/2), (0-w/2,h/2), (w/2,h/2), (w/2,0-h/2) ]
oval w h =
  let n = 50
      f i = (w/2 * cos (2*pi/n * i), h/2 * sin (2*pi/n * i))
  in  Shape $ map f [0..n-1]
circle r = oval (2*r) (2*r)
ngon n r =
  let m = toFloat n
      f i = ( r * cos (2*pi/m * i), r * sin (2*pi/m * i))
  in  Shape $ map f [0..n-1]

data Transform
  = Rotate Float
  | Move Float Float
  | Scale Float Float
  | Transform Float Float Float Float Float Float

data Form = Form [Transform] BasicForm


data LineCap  = Butt  | Round | Square
data LineJoin = Round | Bevel | Miter

data FillStyle
  = NoFill
  | Solid Color
  | Texture String
  | LinearGradient [(Float,Color)] (Float,Float) (Float,Float)
  | RadialGradient [(Float,Color)] (Float,Float) Float (Float,Float) Float

-- dash pattern, dash offset, line width, cap, join, miter limit, fill style
type LineStyle = {
  width : Float,
  cap   : LineCap,
  join  : LineJoin,
  miterLimit : Float,
  fillStyle  : FillStyle,
  dashing    : [Int],
  dashOffset : Int
}

data BasicForm
  = FLine LineStyle Line
  | FShape LineStyle FillStyle Shape
  | FImage Int Int Int Int String
  | FElement Element
  | FGroup [Form]

setFillStyle fs form =
  case form of
    FLine  ls _ ln  -> FLine ls fs ln
    FShape ls _ shp -> FLine ls fs shp
    _ -> form

noFill = setFillStyle NoFill
fillColor clr = setFillStyle (Solid clr)
texture src = setFillStyle (Texture src)
linearGradient stops start end = setFillStyle (LinearGradient stops start end)
radialGradient stops innerP innerR outerP outerR =
    setFillStyle (RadialGradient stops innerP innerR outerP outerR)

plain = { width=1, cap=Butt, join=Miter, miterLimit=10,
          fillStyle=...,
          dashing=[], dashOffset=0 }

solid  c = Form [] . FLine c
dotted c = Form [] . FLine c
dashed c = Form [] . FLine c
lineStyle = Form [] . FLine


filled   clr shp = Form [] (FShape  shp)
outlined clr shp = Form [] (FShape  shp)
customOutline pattern clr (Shape ps pos) =
    Form [] (FShape (CustomOutline pattern) clr (Shape ps pos))
textured src (Shape ps pos) = Form [] (FShape (Textured src) black (Shape ps pos))

fromLine = FLine
fromShape = FShape
fromElement = FElement

sprite w h pos src = Form 0 1 pos (FImage w h src)
toForm pos e = Form 0 1 pos (FElement e)

addTransform t (Form ts form) = Form (t:ts) form

rotateRad rad = addTransform $ Rotate rad
rotateDeg deg = addTransform $ Rotate (pi * deg / 180)
rotate    rot = addTransform $ Rotate (2 * pi * rot)

scale  s = addTransform (Scale s s)
scaleX s = addTransform (Scale s 1)
scaleY s = addTransform (Scale 1 s) 

move x y = addTransform (Move x y)
moveX x  = addTransform (Move x 0)
moveY y  = addTransform (Move 0 y)

transform a b c d e f = addTransform (Transform a b c d e f)