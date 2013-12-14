module MJS where

{-| MJS

-}

import Native.MJS

data V3 = Dummy_V3
data M4x4 = Dummy_M4x4

v3 : Float -> Float -> Float -> V3
v3 = Native.MJS.v3

v3add : V3 -> V3 -> V3
v3add = Native.MJS.v3add

v3length : V3 -> Float
v3length = Native.MJS.v3length

v3normalize : V3 -> V3
v3normalize = Native.MJS.v3normalize

v3scale : V3 -> Float -> V3
v3scale = Native.MJS.v3scale

v3mul4x4 : M4x4 -> V3 -> V3
v3mul4x4 = Native.MJS.v3mul4x4

m4x4identity : M4x4
m4x4identity = Native.MJS.m4x4identity

m4x4mul : M4x4 -> M4x4 -> M4x4
m4x4mul = Native.MJS.m4x4mul

m4x4makeRotate : Float -> V3 -> M4x4
m4x4makeRotate = Native.MJS.m4x4makeRotate

m4x4makeLookAt : V3 -> V3 -> V3 -> M4x4
m4x4makeLookAt = Native.MJS.m4x4makeLookAt

m4x4makePerspective : Float -> Float -> Float -> Float -> M4x4
m4x4makePerspective = Native.MJS.m4x4makePerspective
