module MJS where

{-| MJS

-}

import Native.MJS

data V3 = V3

v3 : Float -> Float -> Float -> V3
v3 = Native.MJS.v3
