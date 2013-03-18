
module Graphics.Collage where

import Either
import Graphics.LineStyle as LS
import Graphics.Geometry
import Native.Graphics.Matrix as Matrix
import Native.Graphics.Collage as N
import Graphics.Element

type Form = { transform : Matrix, form : BasicForm }

data FillStyle
  = Solid Color
  | Texture String
  | Gradient Gradient

data BasicForm
  = FPath LineStyle Path
  | FShape (Either LineStyle FillStyle) Shape
  | FImage Int Int (Int,Int) String
  | FElement Element
  | FGroup [Form]

ident = Matrix.identity

fill style shape = Form ident (FShape (Right style) shape)

filled : Color -> Shape -> Form
filled color shape = fill (Solid color) shape

textured : String -> Shape -> Form
textured src shape = fill (Texture src) shape

gradient : Gradient -> Shape -> Form
gradient grad shape = fill (Gradient grad) shape

outline : LineStyle -> Shape -> Form
outline style shape = Form ident (FShape (Left style) shape)

trace : LineStyle -> Path -> Form
trace style path = Form ident (FPath style path)

sprite : Int -> Int -> (Int,Int) -> String -> Form
sprite w h pos src = Form ident (FImage w h pos src)

toForm : Element -> Form
toForm e = Form ident (FElement e)

group fs = Form ident (FGroup fs)

rotate t f = { form = f.form, transform = Matrix.rotate  t f.transform }
scale  s f = { form = f.form, transform = Matrix.scale s s f.transform }
scaleX s f = { form = f.form, transform = Matrix.scale s 1 f.transform }
scaleY s f = { form = f.form, transform = Matrix.scale 1 s f.transform }
move x y f = { form = f.form, transform = Matrix.move  x y f.transform }
moveX  x f = { form = f.form, transform = Matrix.move  x 0 f.transform }
moveY  y f = { form = f.form, transform = Matrix.move  0 y f.transform }

transform u v w x y z f =
    { form = f.form, transform = Matrix.matrix u v w x y z f.transform }

collage : Int -> Int -> [Form] -> Element
