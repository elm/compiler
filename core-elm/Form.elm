
module Collage where

data Line
    = Start     (Float,Float)
    | Arc       (Float,Float) Float (Float,Float) Path
    | Line      (Float,Float) Path
    | Bezier    (Float,Float) (Float,Float) (Float,Float) Path
    | Quadratic (Float,Float) (Float,Float) Path

path ps = case ps of
            [] -> Start (0,0)
            hd:tl -> foldl Line (Start hd) tl

segment p1 p2 = Line p1 (Start p2)

start       = Start
arcTo       = Arc
lineTo      = Line
bezierTo    = Bezier
quadraticTo = Quadratic

data Shape = Shape Line

polygon ps = Shape (case ps of
                      []    -> Start (0,0)
                      hd:tl -> Line hd $ foldl Line (Start hd) tl)

rect w h = polygon [ (0-w/2,0-h/2), (0-w/2,h/2), (w/2,h/2), (w/2,0-h/2) ]
oval w h =
  let n = 50
      f i = ( w/2 * cos (2*pi/n * i), h/2 * sin (2*pi/n * i))
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

data LineStyle = NoLine | Line [Int] Int Float LineCap LineJoin Float FillStyle

data BasicForm
  = FLine LineStyle FillStyle Line
  | FShape LineStyle FillStyle Shape
  | FImage Int Int String
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

solid  clr ln = Form [] (FLine  ln)
dotted clr ln = Form [] (FLine  ln)
dashed clr ln = Form [] (FLine  ln)
customLine pattern clr ln = Form [] (FLine (Custom pattern) clr ln)


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