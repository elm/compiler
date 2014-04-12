module Graphics.WebGL (mapTriangle, zipTriangle, loadTex, model, webgl) where

{-| The WebGL API is for high performance drawing.

# Triangle Convenience Functions
@docs Triangle, mapTriangle, zipTriangle

# Shaders
Shaders are created using glsl blocks. 

```haskell
vertex : Shader { a | pos : V3 } {} {}
vertex = [glsl|

attribute vec3 pos;

void main () {
  gl_Position = vec4(pos,1.0);
}

\]
```

# WebGL Element
@docs loadTex, model, webgl

-}

import Graphics.Element (Element)
import Http (Response)
import Native.Graphics.WebGL
import Signal (Signal)

unsafeCoerceGLSL : String -> Shader attr unif vary
unsafeCoerceGLSL = Native.Graphics.WebGL.unsafeCoerceGLSL

{-| Triangles are only homogenous 3-tuples. Each element describes something about each vertex, and must be a record to be used with GLSL shaders.
-}
type Triangle a = (a,a,a)

{-| Apply a function to each vertex -}
mapTriangle : (a -> b) -> Triangle a -> Triangle b
mapTriangle f (x,y,z) = (f x, f y, f z)

{-| Apply an arity-2 function element-wise on 2 triangles -}
zipTriangle : (a -> b -> c) -> Triangle a -> Triangle b -> Triangle c
zipTriangle f (x,y,z) (x',y',z') = (f x x', f y y', f z z')

{-| Opaque Shader Type -}
data Shader attr unif vary = Shader

{-| Opaque Texture Type -}
data Texture = Texture

{-| Load an image url as a texture -}
loadTex : String -> Signal (Response Texture)
loadTex = Native.Graphics.WebGL.loadTex

{-| Opaque Model Type -}
data Model = Model 

{-| A model is a collection of a fragment shader, a vertex shader, attribute data, and uniform data. -}
model : Shader attr unif vary -> Shader {} unif vary -> [Triangle attr] -> unif -> Model
model = Native.Graphics.WebGL.model

{-| Render a WebGL scene with the given dimensions and models -}
webgl : (Int,Int) -> [Model] -> Element
webgl = Native.Graphics.WebGL.webgl
