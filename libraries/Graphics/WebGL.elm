module Graphics.WebGL (mapTriangle, zipTriangle, unsafeCoerceGLSL, loadTex, model, webgl) where

{-| The WebGL API is for high performance drawing.

# Triangle Convenience Functions
@docs Triangle, mapTriangle, zipTriangle

# Shaders
@docs Shader

Shaders are created using inline GLSL blocks. The type of the Shader is determined by the top-level attribute, uniform, and varying declarations of the parsed GLSL source code. The type parameters to Shader are records with field names matching the names of the declarations.

```haskell
vertex : Shader { a | pos : V3 } { b | model : M4x4 } {}
vertex = [glsl|

attribute vec3 pos;
uniform mat4 model;

void main () {
  gl_Position = model * vec4(pos,1.0);
}

\]
```

GLSL attribute variables are per-vertex values. For example, it could hold the position or color of a specific vertex of a specific triangle in your model.

GLSL uniform variables are global values which will be applied for all vertices of all triangles of the model.

GLSL varying variables are shared between stages; they are output variables for vertex shaders, and input variables for fragment shaders.

@docs unsafeCoerceGLSL

# Loading Textures
@docs loadTex

# Packaging Models
@docs model

# WebGL Element
@docs webgl

-}

import Graphics.Element (Element)
import Http (Response)
import Native.Graphics.WebGL
import Signal (Signal)

{-| Triangles are only homogenous 3-tuples. Each element describes something about each vertex, and must be a record to be used with GLSL shaders.
-}
type Triangle a = (a,a,a)

{-| Apply a function to each vertex -}
mapTriangle : (a -> b) -> Triangle a -> Triangle b
mapTriangle f (x,y,z) = (f x, f y, f z)

{-| Apply an arity-2 function element-wise on 2 triangles -}
zipTriangle : (a -> b -> c) -> Triangle a -> Triangle b -> Triangle c
zipTriangle f (x,y,z) (x',y',z') = (f x x', f y y', f z z')

data Shader attr unif vary = Shader

{-| Libary writers can use this function to create shader combinators. -}
unsafeCoerceGLSL : String -> Shader attribute uniform varying
unsafeCoerceGLSL = Native.Graphics.WebGL.unsafeCoerceGLSL

data Texture = Texture

{-| Loads a texture from the given url. PNG and JPEG are the preferred and tested-compatible source image format. -}
loadTex : String -> Signal (Response Texture)
loadTex = Native.Graphics.WebGL.loadTex

data Model = Model 

{-| Packages a vertex shader, a fragment shader, triangle data, and uniform variables as a opaque Model.
This is a simple container; however, externally there is no type information. 
-}
model : Shader attribute uniform varying -> Shader {} uniform varying -> [Triangle attribute] -> uniform -> Model
model = Native.Graphics.WebGL.model

{-| Render a WebGL scene with the given dimensions and models -}
webgl : (Int,Int) -> [Model] -> Element
webgl = Native.Graphics.WebGL.webgl
