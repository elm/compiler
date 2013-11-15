module Graphics.WebGL where

{-| WebGL

-}

import Basics (Float)
import Signal (Signal)
import MJS (V3,M4x4)
import Color (Color)
import Graphics.Element (Element)
import Native.Graphics.WebGL

data GLContext = Fake_GLContext

glContext : Int -> Int -> Signal GLContext
glContext = Native.Graphics.WebGL.glContext

type GLPoint = { pos: V3, color: Color }
type GLTriangle = { a: GLPoint, b: GLPoint, c: GLPoint }

data Mesh = Fake_Mesh

makeMesh : [GLTriangle] -> GLContext -> Mesh
makeMesh = Native.Graphics.WebGL.makeMesh

data Scene = Node M4x4 [Scene] | Leaf Mesh

renderGL : GLContext -> Scene -> Element
renderGL = Native.Graphics.WebGL.renderGL

