module MJS where

{-| MJS

-}

import Native.MJS

data V3 = Fake_V3
data M4x4 = Fake_M4x4

v3 : Float -> Float -> Float -> V3
v3 = Native.MJS.v3

m4x4identity : M4x4
m4x4identity = Native.MJS.m4x4identity
