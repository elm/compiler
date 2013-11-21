module Graphics.WebGL (makeGL, Vertex, bufferMesh, transform, mesh, webgl) where

{-| WebGL

-}

import Basics (Float)
import Signal (Signal)
import MJS (V3,M4x4)
import Color (Color)
import Graphics.Element (Element)
import Native.Graphics.WebGL

data GLContext = Fake_Context

makeGL : Int -> Int -> Signal GLContext
makeGL = Native.Graphics.WebGL.makeGL

type Vertex = { pos: V3, color: Color }
type Triangle = (Vertex,Vertex,Vertex)

data Mesh = Fake_Mesh

bufferMesh : [Triangle] -> GLContext -> Mesh
bufferMesh = Native.Graphics.WebGL.bufferMesh

data Scene = SceneNode M4x4 [Scene] | SceneLeaf Mesh

transform : M4x4 -> [Scene] -> Scene
transform = SceneNode

mesh : Mesh -> Scene
mesh = SceneLeaf

webgl : GLContext -> Scene -> Element
webgl = Native.Graphics.WebGL.webgl

