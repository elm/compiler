module Graphics.WebGL (link, mapTriangle, zipTriangle, bind, encapsulate, webgl) where

{-| WebGL -}

import Graphics.Element (Element)
import Native.Graphics.WebGL

type Triangle a = (a,a,a)

mapTriangle : (a -> b) -> Triangle a -> Triangle b
mapTriangle f (x,y,z) = (f x, f y, f z)

zipTriangle : (a -> b -> c) -> Triangle a -> Triangle b -> Triangle c
zipTriangle f (x,y,z) (x',y',z') = (f x x', f y y', f z z')

data Shader attr unif vary = Dummy_Shader

data Model = Dummy_Model 

model : Shader attr unif vary -> Shader {} unif vary -> [Triangle attr] -> unif -> Model
model = Native.Graphics.WebGL.model

webgl : (Int,Int) -> [Model] -> Element
webgl = Native.Graphics.WebGL.webgl
