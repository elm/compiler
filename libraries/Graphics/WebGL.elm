module Graphics.WebGL where

{-| WebGL

-}

import Basics (Float)
import MJS (V3)
import Graphics.Element (Element)
import Native.Graphics.WebGL

type GLTriangle = { a: V3, b: V3, c: V3 }
type GLColor = V3
type Model = [(GLColor,GLTriangle)]

data Transform = NoTransform

data Scene = Node Transform [Scene] | Leaf Model

glContext : Int -> Int -> Scene -> Element
glContext = Native.Graphics.WebGL.glContext
