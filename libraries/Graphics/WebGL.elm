module Graphics.WebGL where

{-| WebGL

-}

import Basics (Float)
import MJS (V3,M4x4)
import Color (Color)
import Graphics.Element (Element)
import Native.Graphics.WebGL

type GLPoint = (V3,Color)
type GLTriangle = { a: GLPoint, b: GLPoint, c: GLPoint }
type Model = [GLTriangle]

data Scene = Node M4x4 [Scene] | Leaf Model

glContext : Int -> Int -> Scene -> Element
glContext = Native.Graphics.WebGL.glContext

