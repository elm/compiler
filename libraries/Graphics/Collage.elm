
module Graphics.Collage where

import Either
import Graphics.LineStyle as LS
import Graphics.Geometry
import Native.Graphics.Matrix as Matrix

type Form = { transform : Matrix, form : BasicForm }

data FillStyle
  = NoFill
  | Solid Color
  | Texture String
  | Gradient Gradient

data BasicForm
  = FLine LineStyle Line
  | FShape (Either LineStyle FillStyle) Shape
  | FImage Int Int (Int,Int) String
  | FElement Element
  | FGroup [Form]

fill style shape = Form [] (FShape (Right style) shape)

filled : Color -> Shape -> Form
filled color shape = fill (Solid color) shape

textured : String -> Shape -> Form
textured src shape = fill (Texture src) shape

gradient : Gradient -> Shape -> Form
gradient grad shape = fill (Gradient grad) shape

outline style shape = Form [] (FShape (Left style) shape)
trace style line = Form [] (FLine style line)

sprite w h pos src = Form [] (FImage w h pos src)
toForm e = Form [] (FElement e)

group fs = Form [] (FGroup fs)

add t f = { transforms = Matrix.multiply t f.transforms, form = f.form }

radians t = t
degrees dep = pi * deg / 180
rotations rot = 2 * pi * rot

rotate t = add (Matrix.rotate t)

scale  s = add (Matrix.scale s s)
scaleX s = add (Matrix.scale s 1)
scaleY s = add (Matrix.scale 1 s) 

move x y = add (Matrix.translate x y)
moveX x  = add (Matrix.translate x 0)
moveY y  = add (Matrix.translate 0 y)

transform a b c d e f = add (Matrix.matrix a b c d e f)