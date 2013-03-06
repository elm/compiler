
module Graphics.Collage where

import Either
import Graphics.LineStyle
import Geometry

data Transform
  = Rotate Float
  | Move Float Float
  | Scale Float Float
  | Transform Float Float Float Float Float Float

data Form = Form [Transform] BasicForm

data FillStyle
  = NoFill
  | Solid Color
  | Texture String
  | Gradient Gradient

data BasicForm
  = FLine LineStyle Line
  | FShape (Either LineStyle FillStyle) Shape
  | FImage Int Int Int Int String
  | FElement Element
  | FGroup [Form]

fill style shape = FShape (Right style) shape

filled : Color -> Shape -> Form
filled color shape = fill (Solid color) shape

textured : String -> Shape -> Form
textured src shape = fill (Texture src) shape

gradient : Gradient -> Shape -> Form
gradient grad shape = fill (Gradient grad) shape

outline style shape = FShape (Left style) shape
trace = FLine


linearGradient stops start end = setFillStyle (LinearGradient stops start end)
radialGradient stops innerP innerR outerP outerR =
    setStyle (RadialGradient stops innerP innerR outerP outerR)

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