
module Graphics.Element (widthOf, heightOf, sizeOf,
                         width, height, opacity, color, tag, link,
                         image, fittedImage, croppedImage,
                         flow, up, down, left, right, inward, outward,
                         above, below, beside, layers,
                         container, absolute, relative,
                         middle, topLeft, topRight, bottomLeft, bottomRight,
                         midLeft, midRight, midTop, midBottom, middleAt,
                         topLeftAt, topRightAt, bottomLeftAt, bottomRightAt,
                         midLeftAt, midRightAt, midTopAt, midBottomAt,
                         spacer, newElement
                        ) where

import Native.Utils (guid, max, htmlHeight)
import JavaScript as JS
import List as List
import Graphics.Color as Color
import Maybe (Just, Nothing)

type Properties = {
  id      : Int,
  width   : Int,
  height  : Int,
  opacity : Float,
  color   : Maybe Color,
  href    : JSString,
  tag     : JSString
 }

type Element = { props : Properties, element : ElementPrim }

widthOf : Element -> Int
widthOf  e = e.props.width

heightOf : Element -> Int
heightOf e = e.props.height

sizeOf : Element -> Int
sizeOf   e = (e.props.width, e.props.height)

width : Int -> Element -> Element
width  nw e = let p = e.props
                  props = case e.element of
                            Image _ w h _ -> {p| height <- h/w*nw }
                            RawHtml html -> {p| height <- let (w,h) = htmlHeight nw html in h}
                            _ -> p
              in { element=e.element, props={props| width <- nw} }

height : Int -> Element -> Element
height nh e = let p = e.props
                  props = case e.element of
                            Image _ w h _ -> {p| width <- w/h*nh }
                            _ -> p
              in { element=e.element, props={p| height  <- nh} }

opacity : Float -> Element -> Element
opacity o e = let p = e.props in { element=e.element, props={p| opacity <- o} }

color : Color -> Element -> Element
color   c e = let p = e.props in
              { element=e.element, props={p| color <- Just c} }

tag : String -> Element -> Element
tag  name e = let p = e.props in
              { element=e.element, props={p| tag   <- JS.fromString name} }

link : String -> Element -> Element
link href e = let p = e.props in
              { element=e.element, props={p| href  <- JS.fromString href} }

emptyStr = JS.fromString ""
newElement w h e =
  { props = Properties (guid ()) w h 1 Nothing emptyStr emptyStr, element = e }

data ElementPrim
  = Image ImageStyle Int Int JSString
  | Container Position Element
  | Flow Direction [Element]
  | Spacer
  | RawHtml JSString
  | Custom -- for custom Elements implemented in JS, see collage for example

data ImageStyle = Plain | Fitted | Cropped (Int,Int)

image : Int -> Int -> String -> Element
image w h src = newElement w h (Image Plain w h (JS.fromString src))

fittedImage : Int -> Int -> String -> Element
fittedImage w h src = newElement w h (Image Fitted w h (JS.fromString src))

croppedImage : Int -> Int -> (Int,Int) -> String -> Element
croppedImage w h pos src =
    newElement w h (Image (Cropped pos) w h (JS.fromString src))

data Three = P | Z | N
data Pos = Absolute Int | Relative Float
type Position = { horizontal : Three, vertical : Three, x : Pos, y : Pos }

container : Int -> Int -> Position -> Element -> Element
container w h pos e = newElement w h (Container pos e)

spacer : Int -> Int -> Element
spacer w h = newElement w h Spacer

data Direction = DUp | DDown | DLeft | DRight | DIn | DOut

flow : Direction -> [Element] -> Element
flow dir es =
  let ws = List.map widthOf es
      hs = List.map heightOf es
      newFlow w h = newElement w h (Flow dir es)
  in 
  if es == [] then spacer 0 0 else
  case dir of
    DUp    -> newFlow (List.maximum ws) (List.sum hs)
    DDown  -> newFlow (List.maximum ws) (List.sum hs)
    DLeft  -> newFlow (List.sum ws) (List.maximum hs)
    DRight -> newFlow (List.sum ws) (List.maximum hs)
    DIn    -> newFlow (List.maximum ws) (List.maximum hs)
    DOut   -> newFlow (List.maximum ws) (List.maximum hs)

above : Element -> Element -> Element
above hi lo =
    newElement (max (widthOf hi) (widthOf lo))
               (heightOf hi + heightOf lo)
               (Flow DDown [hi,lo])

below : Element -> Element -> Element
below lo hi =
    newElement (max (widthOf hi) (widthOf lo))
               (heightOf hi + heightOf lo)
               (Flow DDown [hi,lo])

beside : Element -> Element -> Element
beside lft rht =
    newElement (widthOf lft + widthOf rht)
               (max (heightOf lft) (heightOf rht))
               (Flow right [lft,rht])

layers : [Element] -> Element
layers es = 
  let ws = List.map widthOf es
      hs = List.map heightOf es
  in  newElement (List.maximum ws) (List.maximum hs) (Flow DOut es)


-- Repetitive things --

absolute : Int -> Pos
absolute = Absolute
relative : Float -> Pos
relative = Relative

middle      : Position
middle      = { horizontal=Z, vertical=Z, x=Relative 0.5, y=Relative 0.5 }
topLeft     : Position
topLeft     = { horizontal=N, vertical=P, x=Absolute 0, y=Absolute 0 }
topRight    : Position
topRight    = { topLeft | horizontal <- P }
bottomLeft  : Position
bottomLeft  = { topLeft | vertical <- N }
bottomRight : Position
bottomRight = { bottomLeft | horizontal <- P }
midLeft     : Position
midLeft     = { middle  | horizontal <- N, x <- Absolute 0 }
midRight    : Position
midRight    = { midLeft | horizontal <- P }
midTop      : Position
midTop      = { middle  | vertical <- P, y <- Absolute 0 }
midBottom   : Position
midBottom   = { midTop  | vertical <- N }

middleAt          : Pos -> Pos -> Position
middleAt      x y = { horizontal = Z, vertical = Z, x = x, y = y }
topLeftAt         : Pos -> Pos -> Position
topLeftAt     x y = { horizontal = N, vertical = P, x = x, y = y }
topRightAt        : Pos -> Pos -> Position
topRightAt    x y = { horizontal = P, vertical = P, x = x, y = y }
bottomLeftAt      : Pos -> Pos -> Position
bottomLeftAt  x y = { horizontal = N, vertical = N, x = x, y = y }
bottomRightAt     : Pos -> Pos -> Position
bottomRightAt x y = { horizontal = P, vertical = N, x = x, y = y }
midLeftAt         : Pos -> Pos -> Position
midLeftAt     x y = { horizontal = N, vertical = Z, x = x, y = y }
midRightAt        : Pos -> Pos -> Position
midRightAt    x y = { horizontal = P, vertical = Z, x = x, y = y }
midTopAt          : Pos -> Pos -> Position
midTopAt      x y = { horizontal = Z, vertical = P, x = x, y = y }
midBottomAt       : Pos -> Pos -> Position
midBottomAt   x y = { horizontal = Z, vertical = N, x = x, y = y }

up : Direction
up = DUp

down : Direction
down = DDown

left : Direction
left = DLeft

right : Direction
right = DRight

inward : Direction
inward = DIn

outward : Direction
outward = DOut