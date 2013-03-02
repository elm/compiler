
module Grapics.Element where

import Native.Guid (guid)
import JavaScript as JS
import List as List
import Graphics.FillStyle

type Properties = {
  id      : Int,
  width   : Int,
  height  : Int,
  opacity : Float,
  filling : FillStyle,
  href    : JSString,
  tag     : JSString
 }

data Element = Element Properties ElementPrim

getProp get e = let (Element props _) = e in get props
widthOf  e = getProp .width e
heightOf e = getProp .height e
sizeOf   e = getProp (\p -> (p.width, p.height)) e

width w e =
  case e of
    Element props prim -> Element {props| width   <- w} prim
height h e =
  case e of
    Element props prim -> Element {props| height  <- h} prim
opacity o e =
  case e of
    Element props prim -> Element {props| opacity <- o} prim
color c e =
  case e of
    Element props prim -> Element {props| filling <- c} prim
filling f e =
  case e of
    Element props prim -> Element {props| filling <- f} prim
tag name e =
  case e of
    Element props prim -> Element {props| tag  <- JS.fromString name } prim
link href e =
  case e of
    Element props prim -> Element {props| href <- JS.fromString href } prim

emptyStr = JS.fromString ""
newElement w h e =
  Element (Properties (guid ()) w h 1 NoFill emptyStr emptyStr) e

data ElementPrim
  = Image ImageStyle String
  | Container Position Element
  | Flow Direction [Element]
  | Spacer
  | RawHtml JSString
  | DomNode JSElement

data ImageStyle = Plain | Fitted | Cropped (Int,Int) Int Int
data Direction = DUp | DDown | DLeft | DRight | DIn | DOut

image w h src = newElement w h (Image Plain src)
fittedImage w h src = newElement w h (Image Fitted src)
croppedImage w h pos src = newElement w h (Image (Cropped pos w h) src)

container w h pos e = newElement w h (Container pos e)
spacer w h = newElement w h Spacer

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

text : Text -> Element