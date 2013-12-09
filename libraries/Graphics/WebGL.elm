module Graphics.WebGL (link, bind, encapsulate, webgl) where

{-| WebGL -}

import Graphics.Element (Element)
import Native.Graphics.WebGL
import Signal (Signal)

data GL = Dummy_GL

data Shader auv = Dummy_Shader
data Program a u = Dummy_Program

link : GL -> Shader { attribute : a, uniform : u, varying : v } -> Shader { attribute : {}, uniform : {}, varying : v } -> Program a u
link = Native.Graphics.WebGL.link

-- Binder really should not need a program
-- I need runtime type information from something though...

data Buffer a = Dummy_Buffer

bind : GL -> Program a u -> [a] -> Buffer a
bind = Native.Graphics.WebGL.bind

-- Now I cheat here because elm lacks existential types or rank-n
-- I'm in essense doing the following
-- data Model = forall a u. Model (Program a u) (Buffer a) u

data Model = Dummy_Model 

encapsulate : Program a u -> Buffer a -> u -> Model
encapsulate = Native.Graphics.WebGL.encapsulate

webgl : Signal (Int,Int) -> Signal (GL -> [Model]) -> Signal Element
webgl = Native.Graphics.WebGL.webgl
