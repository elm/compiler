module Graphics.Collage where

{-| The collage API is for freeform graphics. You can move, rotate, scale, etc.
all sorts of forms including lines, shapes, images, and elements.

# Unstructured Graphics
@docs collage
 
# Creating Forms
@docs toForm, filled, textured, gradient, outlined, traced
 
# Transforming Forms
@docs move, moveX, moveY, scale, rotate, alpha
 
# Grouping Forms
Grouping forms makes it easier to write modular graphics code. You can create
a form that is a composite of many subforms. From there it is easy to transform
it as a single unit.

@docs group, groupTransform
 
# Shapes
@docs rect, oval, square, circle, ngon, polygon

# Paths
@docs segment, path
 
# Line Styles
@docs solid, dashed, dotted, LineStyle, LineCap, LineJoin, defaultLine

-}

import Basics (..)
import List
import Either (Either(Left, Right))
import Transform2D (Transform2D)
import Transform2D as T
import Native.Graphics.Collage
import Graphics.Element (Element)
import Color (Color, black, Gradient)
import Maybe (Maybe)

type alias Form =
    { theta : Float
    , scale : Float
    , x : Float
    , y : Float
    , alpha : Float
    , form : BasicForm
    }

type FillStyle
    = Solid Color
    | Texture String
    | Grad Gradient

{-| The shape of the ends of a line. -}
type LineCap = Flat | Round | Padded

{-| The shape of the &ldquo;joints&rdquo; of a line, where each line segment
meets. `Sharp` takes an argument to limit the length of the joint. This
defaults to 10.
-}
type LineJoin = Smooth | Sharp Float | Clipped

{-| All of the attributes of a line style. This lets you build up a line style
however you want. You can also update existing line styles with record updates.
-}
type alias LineStyle =
    { color : Color
    , width : Float
    , cap   : LineCap
    , join  : LineJoin
    , dashing : [Int]
    , dashOffset : Int
    }

{-| The default line style, which is solid black with flat caps and sharp joints.
You can use record updates to build the line style you
want. For example, to make a thicker line, you could say:

        { defaultLine | width <- 10 }
-}
defaultLine : LineStyle
defaultLine =
    { color = black
    , width = 1
    , cap   = Flat
    , join  = Sharp 10
    , dashing = []
    , dashOffset = 0
    }

{-| Create a solid line style with a given color. -}
solid : Color -> LineStyle
solid  clr = { defaultLine | color <- clr }

{-| Create a dashed line style with a given color. Dashing equals `[8,4]`. -}
dashed : Color -> LineStyle
dashed clr = { defaultLine | color <- clr, dashing <- [8,4] }

{-| Create a dotted line style with a given color. Dashing equals `[3,3]`. -}
dotted : Color -> LineStyle
dotted clr = { defaultLine | color <- clr, dashing <- [3,3] }

type BasicForm
    = FPath LineStyle Path
    | FShape (Either LineStyle FillStyle) Shape
    | FImage Int Int (Int,Int) String
    | FElement Element
    | FGroup Transform2D [Form]

form : BasicForm -> Form
form f = { theta=0, scale=1, x=0, y=0, alpha=1, form=f }

fill style shape = form (FShape (Right style) shape)

{-| Create a filled in shape. -}
filled : Color -> Shape -> Form
filled color shape = fill (Solid color) shape

{-| Create a textured shape. The texture is described by some url and is
tiled to fill the entire shape.
-}
textured : String -> Shape -> Form
textured src shape = fill (Texture src) shape

{-| Fill a shape with a [gradient](/library/Color.elm#linear). -}
gradient : Gradient -> Shape -> Form
gradient grad shape = fill (Grad grad) shape

{-| Outline a shape with a given line style. -}
outlined : LineStyle -> Shape -> Form
outlined style shape = form (FShape (Left style) shape)

{-| Trace a path with a given line style. -}
traced : LineStyle -> Path -> Form
traced style path = form (FPath style path)

{-| Create a sprite from a sprite sheet. It cuts out a rectangle
at a given position.
-}
sprite : Int -> Int -> (Int,Int) -> String -> Form
sprite w h pos src = form (FImage w h pos src)

{-| Turn any `Element` into a `Form`. This lets you use text, gifs, and video
in your collage. This means you can move, rotate, and scale
an `Element` however you want.
-}
toForm : Element -> Form
toForm e = form (FElement e)

{-| Flatten many forms into a single `Form`. This lets you move and rotate them
as a single unit, making it possible to build small, modular components.
-}
group : [Form] -> Form
group fs = form (FGroup T.identity fs)

{-| Flatten many forms into a single `Form` and then apply a matrix
transformation.
-}
groupTransform : Transform2D -> [Form] -> Form
groupTransform matrix fs = form (FGroup matrix fs)

{-| Move a form by the given amount. This is a relative translation so
`(move (10,10) form)` would move `form` ten pixels up and ten pixels to the
right.
-}
move : (Float,Float) -> Form -> Form
move (x,y) f = { f | x <- f.x + x, y <- f.y + y }

{-| Move a shape in the x direction. This is relative so `(moveX 10 form)` moves
`form` 10 pixels to the right.
-}
moveX : Float -> Form -> Form
moveX x f = { f | x <- f.x + x }

{-| Move a shape in the y direction. This is relative so `(moveY 10 form)` moves
`form` upwards by 10 pixels.
-}
moveY : Float -> Form -> Form
moveY y f = { f | y <- f.y + y }

{-| Scale a form by a given factor. Scaling by 2 doubles the size.
-}
scale : Float -> Form -> Form
scale s f = { f | scale <- f.scale * s }

{-| Rotate a form by a given angle. Rotate takes standard Elm angles (radians)
and turns things counterclockwise. So to turn `form` 30&deg; to the left
you would say, `(rotate (degrees 30) form)`.
-}
rotate : Float -> Form -> Form
rotate t f = { f | theta <- f.theta + t }

{-| Set the alpha of a `Form`. The default is 1, and 0 is totally transparent. -}
alpha : Float -> Form -> Form
alpha a f = { f | alpha <- a }

{-| A collage is a collection of 2D forms. There are no strict positioning
relationships between forms, so you are free to do all kinds of 2D graphics.
-}
collage : Int -> Int -> [Form] -> Element
collage = Native.Graphics.Collage.collage


type alias Path = [(Float,Float)]

{-| Create a path that follows a sequence of points. -}
path : [(Float,Float)] -> Path
path ps = ps

{-| Create a path along a given line segment. -}
segment : (Float,Float) -> (Float,Float) -> Path
segment p1 p2 = [p1,p2]

type alias Shape = [(Float,Float)]

{-| Create an arbitrary polygon by specifying its corners in order.
`polygon` will automatically close all shapes, so the given list
of points does not need to start and end with the same position.
-}
polygon : [(Float,Float)] -> Shape
polygon points = points

{-| A rectangle with a given width and height. -}
rect : Float -> Float -> Shape
rect w h = let hw = w/2
               hh = h/2
           in  [ (0-hw,0-hh), (0-hw,hh), (hw,hh), (hw,0-hh) ]

{-| A square with a given edge length. -}
square : Float -> Shape
square n = rect n n

{-| An oval with a given width and height. -}
oval : Float -> Float -> Shape
oval w h =
  let n = 50
      t = 2 * pi / n
      hw = w/2
      hh = h/2
      f i = (hw * cos (t*i), hh * sin (t*i))
  in  List.map f [0..n-1]

{-| A circle with a given radius. -}
circle : Float -> Shape
circle r = oval (2*r) (2*r)

{-| A regular polygon with N sides. The first argument specifies the number
of sides and the second is the radius. So to create a pentagon with radius
30 you would say:

        ngon 5 30
-}
ngon : Int -> Float -> Shape
ngon n r =
  let m = toFloat n
      t = 2 * pi / m
      f i = ( r * cos (t*i), r * sin (t*i) )
  in  List.map f [0..m-1]
