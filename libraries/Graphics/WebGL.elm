module Graphics.WebGL (encapsulate, webgl) where

{-| WebGL -}

import Graphics.Element (Element)
import Native.Graphics.WebGL

data Shader a u v = Dummy_Shader
data Program a u = Dummy_Program
type Linker = Shader a u v -> Shader {} {} v -> Program a u

-- Binder really should not need a program
-- I need runtime type information from something though...

data Buffer a = Dummy_Buffer
type Binder = Program a u -> [a] -> Buffer a

-- Now I cheat here because elm lacks existential types or rank-n
-- I'm in essense doing the following
-- data Model = forall a u. Model (Program a u) (Buffer a) u

data Model = Dummy_Model 

encapsulate : Program a u -> Buffer a -> u -> Model
encapsulate = Native.Graphics.WebGL.encapsulate

webgl : Int -> Int -> (Linker -> Binder -> [Model]) -> Element
webgl = Native.Graphics.WebGL.webgl
