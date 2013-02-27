
module Graphics.Element
    (left,right,down,up,inward,outward
    ,topLeft,topRight,bottomLeft,bottomRight
    ,midLeft,midRight,midTop,midBottom
    ,middle,absolute,relative
    ,topLeftAt,topRightAt,bottomLeftAt,bottomRightAt
    ,width,height,size,widthOf,heightOf,sizeOf
    ,color,opacity
    ,text,image,fittedImage,video
    ,flow,above,below,beside,layers
    ,collage,spacer,container
    ,line,segment
    ,polygon,ngon,rect,oval,circle
    ,solid,dotted,dashed,customLine
    ,filled,outlined,customOutline,textured
    ,sprite,toForm
    ,rotate,scale,move
    ) where

import Graphics.Color

data Direction = DLeft | DRight | DUp | DDown | DIn | DOut

left    = DLeft
right   = DRight
down    = DDown
up      = DUp
inward  = DIn
outward = DOut

data Pos1 = Absolute Int | Relative Float
data Pos2 = Near | Mid | Far

data Position
  = Position Pos2 Pos2
  | PositionAt Pos1 Pos1
  | PositionTL Pos1 Pos1
  | PositionTR Pos1 Pos1
  | PositionBL Pos1 Pos1
  | PositionBR Pos1 Pos1

topLeft     = Position Near Near
topRight    = Position Far  Near
bottomLeft  = Position Near Far
bottomRight = Position Far  Far

midLeft     = Position Near Mid
midRight    = Position Far  Mid
midTop      = Position Mid  Near
midBottom   = Position Mid  Far

middle      = Position Mid  Mid

middleAt      = PositionAt
topLeftAt     = PositionTL
topRightAt    = PositionTR
bottomLeftAt  = PositionBL
bottomRightAt = PositionBR

absolute = Absolute
relative = Relative

data Element = Element Int BasicElement Int Int Float (Maybe Color) (Just String)

newElement = Element 0
basicNewElement e w h = Element 0 e w h 1 Nothing Nothing

data BasicElement
  = EText Text Text
  | EImage String
  | EVideo String
  | EFittedImage String
  | EFlow Direction [Element]
  | ECollage Int Int [Form]
  | EEmpty
  | EContainer Position Element

width w' (Element _ e w h o c l) =
  case e of
  { EText  _ -> newElement e w' h o c l
  ; EImage _ -> newElement e w' h o c l
  ; EVideo _ -> newElement e w' h o c l
  ; _        -> newElement e w' h o c l }
height  h' (Element _ e w h o c l) =
  case e of
  { EText  _ -> newElement e w h' o c l
  ; EImage _ -> newElement e w h' o c l
  ; EVideo _ -> newElement e w h' o c l
  ; _        -> newElement e w h' o c l }
size w' h' (Element _ e w h o c l) = newElement e w' h' o  c l
opacity o' (Element _ e w h o c l) = newElement e w  h  o' c l
color   c' (Element _ e w h o c l) = newElement e w  h  o  (Just c') l
link    l' (Element _ e w h o c l) = newElement e w  h  o  c (Just l')

widthOf  (Element _ _ w _ _ _ _) = w
heightOf (Element _ _ _ h _ _ _) = h
sizeOf   (Element _ _ w h _ _ _) = (w,h)

n1 = 0-1

text t  = basicNewElement (EText t)  n1 n1
image w h s = basicNewElement (EImage s) w h
video w h s = basicNewElement (EVideo s) w h
fittedImage w h s = basicNewElement (EFittedImage s) w h

flow dir es =
  let { w = let ws = map widthOf es in
            case dir of { DLeft  -> sum ws
                        ; DRight -> sum ws
                        ; _      -> maximum ws }
      ; h = let hs = map heightOf es in
            case dir of { DDown -> sum hs
                        ; DUp   -> sum hs
                        ; _     -> maximum hs }
  } in  basicNewElement (EFlow dir es) w h

above e1 e2 =
  newElement
    (EFlow DDown [e1,e2])
    (max (widthOf e1) (widthOf e2))
    (heightOf e1 + heightOf e2) 1 Nothing Nothing

below e1 e2 =
  basicNewElement (EFlow DDown [e2,e1])
    (max (widthOf e1) (widthOf e2))
    (heightOf e1 + heightOf e2)

beside e1 e2 =
  basicNewElement (EFlow DRight [e1,e2])
    (widthOf e1 + widthOf e2)
    (max (heightOf e1) (heightOf e2))

layers es =
  basicNewElement (EFlow DIn es)
    (maximum (map widthOf es))
    (maximum (map heightOf es))

collage w h forms = basicNewElement (ECollage w h forms) w h

spacer w h = basicNewElement EEmpty w h

container w h pos e =
  basicNewElement (EContainer pos e) w h



data LineStyle = Solid | Dotted | Dashed | Custom [Int]
data ShapeStyle = Filled | Outlined | CustomOutline [Int] | Textured String

data Line = Line [(Float,Float)]

line = Line
segment p1 p2 = Line [p1,p2]

data Shape = Shape [(Float,Float)] (Float,Float)

polygon = Shape
rect w h pos = Shape [(0-w/2,0-h/2),(0-w/2,h/2),(w/2,h/2),(w/2,0-h/2)] pos
oval w h pos =
  let n = 50 in
  let f i = ( w/2 * cos (2*pi/n * i), h/2 * sin (2*pi/n * i)) in
    Shape (map f [0..n-1]) pos
circle r = oval (2*r) (2*r)
ngon n r pos =
  let m = toFloat n in
  let f i = ( r * cos (2*pi/m * i), r * sin (2*pi/m * i)) in
    Shape (map f [0..n-1]) pos

data Form = Form Float Float (Float,Float) BasicForm

data BasicForm
  = FLine LineStyle Color Line
  | FShape ShapeStyle Color Shape
  | FImage Int Int String
  | FElement Element

solid  clr ln = Form 0 1 (0,0) (FLine Solid  clr ln)
dotted clr ln = Form 0 1 (0,0) (FLine Dotted clr ln)
dashed clr ln = Form 0 1 (0,0) (FLine Dashed clr ln)
customLine pattern clr ln =
    Form 0 1 (0,0) (FLine (Custom pattern) clr ln)

filled   clr (Shape ps pos) = Form 0 1 pos (FShape Filled   clr (Shape ps pos))
outlined clr (Shape ps pos) = Form 0 1 pos (FShape Outlined clr (Shape ps pos))
customOutline pattern clr (Shape ps pos) =
    Form 0 1 pos (FShape (CustomOutline pattern) clr (Shape ps pos))
textured src (Shape ps pos) = Form 0 1 pos (FShape (Textured src) black (Shape ps pos))

sprite w h pos src = Form 0 1 pos (FImage w h src)
toForm pos e = Form 0 1 pos (FElement e)

rotate t (Form theta scale   pos   form) = Form (t+theta) scale pos form
scale s  (Form theta scale   pos   form) = Form theta (s*scale) pos form
move x y (Form theta scale (px,py) form) = Form theta scale (x+px,y+py) form