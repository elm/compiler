module Graphics.WebGL where

{-| WebGL

-}

import Basics (Float)
import Graphics.Element (Element)
import Native.Graphics.WebGL

type GLPoint = { x: Float, y: Float, z: Float }
type GLColor = { r: Int, g: Int, b: Int, a: Int }
type GLTriangle = { a: GLPoint, b: GLPoint, c: GLPoint }
type Model = [(GLColor,GLTriangle)]

data Transform = NoTransform

data Scene = Node Transform [Scene] | Leaf Model

glContext : Int -> Int -> Scene -> Element
glContext = Native.Graphics.WebGL.glContext
