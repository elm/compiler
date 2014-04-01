module Graphics.WebGL (mapTriangle, zipTriangle, loadTex, model, webgl) where

{-| The WebGL API is for high performance drawing.

# Triangle Convenience Functions
@docs Triangle, mapTriangle, zipTriangle

# Shaders
Shaders are created using glsl blocks. 

vertex : Shader { a | pos : V3 } {} {}
vertex = [glsl|

attribute vec3 pos;

void main () {
  gl_Position = vec4(pos,1.0);
}

|]

# WebGL Element
@docs webgl

-}

import Graphics.Element (Element)
import Http (Response)
import Native.Graphics.WebGL
import Signal (Signal)

unsafeCoerceGLSL : String -> Shader attr unif vary
unsafeCoerceGLSL = Native.Graphics.WebGL.unsafeCoerceGLSL

type Triangle a = (a,a,a)

mapTriangle : (a -> b) -> Triangle a -> Triangle b
mapTriangle f (x,y,z) = (f x, f y, f z)

zipTriangle : (a -> b -> c) -> Triangle a -> Triangle b -> Triangle c
zipTriangle f (x,y,z) (x',y',z') = (f x x', f y y', f z z')

data Shader attr unif vary = Shader

data Texture = Texture

loadTex : String -> Signal (Response Texture)
loadTex = Native.Graphics.WebGL.loadTex

data Model = Model 

model : Shader attr unif vary -> Shader {} unif vary -> [Triangle attr] -> unif -> Model
model = Native.Graphics.WebGL.model

webgl : (Int,Int) -> [Model] -> Element
webgl = Native.Graphics.WebGL.webgl
