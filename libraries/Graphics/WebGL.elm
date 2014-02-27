
module Graphics.WebGL (mapTriangle, zipTriangle, loadTex, model, webgl) where

{-| WebGL -}

import Graphics.Element (Element)
import Http (Response)
import Native.Graphics.WebGL
import Signal (Signal)

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
