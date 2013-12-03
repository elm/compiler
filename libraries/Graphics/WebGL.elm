module Graphics.WebGL (makeGL, linkProgram, linkModel, webgl) where

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

data GLShader declarations = Fake_GLShader
data GLProgram attributes = Fake_Program

linkProgram : GLShader { attribute : a, uniform : u, varying : v } -> GLShader { attribute : {}, uniform : {}, varying : v } -> GLContext -> GLProgram { attribute : a, uniform : u }
linkProgram = Native.Graphics.WebGL.linkProgram

data GLModel = Fake_Model 

linkModel : (GLProgram { attribute : a, uniform : u }) -> u -> [a] -> GLModel
linkModel = Native.Graphics.WebGL.linkModel

webgl : GLContext -> [GLModel] -> Element
webgl = Native.Graphics.WebGL.webgl

