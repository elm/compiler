
module Grapics.Element (widthOf, heightOf, sizeOf,
                        width, height, opacity, color, tag, link,
                        image, fittedImage, croppedImage,
                        flow, up, down, left, right, inward, outward,
                        above, below, beside, layers,
                        container, absolute, relative,
                        middle, topLeft, topRight, bottomLeft, bottomRight,
                        midLeft, midRight, midTop, midBottom, middleAt,
                        topLeftAt, topRightAt, bottomLeftAt, bottomRightAt,
                        midLeftAt, midRightAt, midTopAt, midBottomAt,
                        spacer
                       ) where

import Native.Utils (guid)
import JavaScript as JS
import List as List
import Graphics.Color

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

widthOf  e = e.props.width
heightOf e = e.props.height
sizeOf   e = (e.props.width, e.props.height)

width   w e = let p = e.props in { element=e.element, props={p| width   <- w} }
height  h e = let p = e.props in { element=e.element, props={p| height  <- h} }
opacity o e = let p = e.props in { element=e.element, props={p| opacity <- o} }
color   c e = let p = e.props in { element=e.element, props={p| color   <- c} }
tag  name e = let p = e.props in
              { element=e.element, props={p| tag  <- JS.fromString name} }
link href e = let p = e.props in
              { element=e.element, props={p| href <- JS.fromString href} }

emptyStr = JS.fromString ""
newElement w h e =
  { props = Properties (guid ()) w h 1 NoFill emptyStr emptyStr, element = e }

data ElementPrim
  = Image ImageStyle Int Int JSString
  | Container Position Element
  | Flow Direction [Element]
  | Spacer
  | RawHtml JSString
  | DomNode JSElement

data ImageStyle = Plain | Fitted | Cropped (Int,Int)

image w h src = newElement w h (Image Plain w h (JS.fromString src))
fittedImage w h src = newElement w h (Image Fitted w h (JS.fromString src))
croppedImage w h pos src =
    newElement w h (Image (Cropped pos) w h (JS.fromString src))

data Three = P | Z | N
data Pos = Absolute Int | Relative Float
type Position = { horizontal : Three, vertical : Three, x : Pos, y : Pos }

container w h pos e = newElement w h (Container pos e)
spacer w h = newElement w h Spacer

data Direction = DUp | DDown | DLeft | DRight | DIn | DOut

flow dir es =
  let ws = map widthOf es
      hs = map heightOf es
      newFlow w h = newElement w h (Flow dir es)
  in 
  case dir of
    DUp    -> newFlow (List.maximum ws) (List.sum hs)
    DDown  -> newFlow (List.maximum ws) (List.sum hs)
    DLeft  -> newFlow (List.sum ws) (List.maximum hs)
    DRight -> newFlow (List.sum ws) (List.maximum hs)
    DIn    -> newFlow (List.maximum ws) (List.maximum hs)
    DOut   -> newFlow (List.maximum ws) (List.maximum hs)

above hi lo = newElement (max (widthOf hi) (widthOf lo)) (heightOf hi + heightOf lo) (Flow DDown [hi,lo])
below lo hi = newElement (max (widthOf hi) (widthOf lo)) (heightOf hi + heightOf lo) (Flow DDown [hi,lo])
beside lft rht = newElement (widthOf lft + widthOf rht) (max (heightOf lft) (heightOf rht)) (Flow right [lft,rht])

layers es = 
  let ws = map widthOf es
      hs = map heightOf es
  in  newElement (List.maximum w) (List.maximum h) (Flow DOut es)


-- Repetitive things --

absolute = Absolute
relative = Relative

middle      = { horizontal = Z, vertical = Z, x = Relative 0.5, y = Relative 0.5 }
topLeft     = { horizontal = N, vertical = P, x = Absolute 0, y = Absolute 0 }
topRight    = { topLeft | horizontal <- P }
bottomLeft  = { topLeft | vertical <- N }
bottomRight = { bottomLeft | horizontal <- P }
midLeft     = { middle  | horizontal <- N, x <- Absolute 0 }
midRight    = { midLeft | horizontal <- P }
midTop      = { middle  | vertical <- P, y <- Absolute 0 }
midBottom   = { midTop  | vertical <- N }
middleAt      x y = { horizontal = Z, vertical = Z, x = x, y = y }
topLeftAt     x y = { horizontal = N, vertical = P, x = x, y = y }
topRightAt    x y = { horizontal = P, vertical = P, x = x, y = y }
bottomLeftAt  x y = { horizontal = N, vertical = N, x = x, y = y }
bottomRightAt x y = { horizontal = P, vertical = N, x = x, y = y }
midLeftAt     x y = { horizontal = N, vertical = Z, x = x, y = y }
midRightAt    x y = { horizontal = P, vertical = Z, x = x, y = y }
midTopAt      x y = { horizontal = Z, vertical = P, x = x, y = y }
midBottomAt   x y = { horizontal = Z, vertical = N, x = x, y = y }

up = DUp
down = DDown
left = DLeft
right = DRight
inward = DIn
outward = DOut