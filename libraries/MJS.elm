module MJS where

{-| MJS

A high performance linear algrbra library using
native JS array types. Geared towards 3D graphics
and used primarily by the Graphics.WebGL.

Both vectors and matrices are immutable.

-}

import Native.MJS

data V3 = Dummy_V3

v3 : Float -> Float -> Float -> V3
v3 = Native.MJS.v3

add : V3 -> V3 -> V3
add = Native.MJS.v3add

sub : V3 -> V3 -> V3
sub = Native.MJS.v3sub

neg : V3 -> V3
neg = Native.MJS.v3neg

{- The normalized direction from v1 to v2 -}
direction : V3 -> V3 -> V3
direction = Native.MJS.v3direction

length : V3 -> Float
length = Native.MJS.v3length

lengthSquared : V3 -> Float
lengthSquared = Native.MJS.v3lengthSquared

normalize : V3 -> V3
normalize = Native.MJS.v3normalize

scale : V3 -> Float -> V3
scale = Native.MJS.v3scale

dot : V3 -> V3 -> Float
dot = Native.MJS.v3dot

cross : V3 -> V3 -> V3
cross = Native.MJS.v3cross

data M4x4 = Dummy_M4x4

mul4x4 : M4x4 -> V3 -> V3
mul4x4 = Native.MJS.v3mul4x4

--m4x4 : V3[16] -> M4x4 ?

identity : M4x4
identity = Native.MJS.m4x4identity

--topLeft3x3 : M4x4 -> M3x3 ? 

{- Assumes that m is orthonormal -}
inverseOrthonormal : M4x4 -> M4x4
inverseOrthonormal = Native.MJS.m4x4inverseOrthonormal

--inverseTo3x3 : M4x4 -> M3x3 ?

makeFrustrum : Float -> Float -> Float -> Float -> Float -> Float -> Float -> M4x4
makeFrustrum = Native.MJS.m4x4makeFrustrum

makePerspective : Float -> Float -> Float -> Float -> M4x4
makePerspective = Native.MJS.m4x4makePerspective

makeOrtho : Float -> Float -> Float -> Float -> Float -> Float -> Float -> M4x4
makeOrtho = Native.MJS.m4x4makeOrtho

makeOrtho2D : Float -> Float -> Float -> Float -> M4x4
makeOrtho2D = Native.MJS.m4x4makeOrtho2D

mul : M4x4 -> M4x4 -> M4x4
mul = Native.MJS.m4x4mul

mulAffine : M4x4 -> M4x4 -> M4x4
mulAffine = Native.MJS.m4x4mulAffine

makeRotate : Float -> V3 -> M4x4
makeRotate = Native.MJS.m4x4makeRotate

rotate : Float -> V3 -> M4x4 -> M4x4
rotate = Native.MJS.m4x4rotate

makeScale3 : Float -> Float -> Float -> M4x4
makeScale3 = Native.MJS.m4x4makeScale3

makeScale : V3 -> M4x4
makeScale = Native.MJS.m4x4makeScale

scaleM3 : Float -> Float -> Float -> M4x4 -> M4x4
scaleM3 = Native.MJS.m4x4scale3

scaleM : V3 -> M4x4 -> M4x4
scaleM = Native.MJS.m4x4scale

makeTranslate3 : Float -> Float -> Float -> M4x4
makeTranslate3 = Native.MJS.m4x4makeTranslate3

makeTranslate : V3 -> M4x4
makeTranslate = Native.MJS.m4x4makeTranslate

translate3 : Float -> Float -> Float -> M4x4 -> M4x4
translate3 = Native.MJS.m4x4translate3

translate : V3 -> M4x4 -> M4x4
translate = Native.MJS.m4x4translate

makeLookAt : V3 -> V3 -> V3 -> M4x4
makeLookAt = Native.MJS.m4x4makeLookAt

transpose : M4x4 -> M4x4
transpose = Native.MJS.m4x4transpose

{- Custom constructor for supplying arbitrary x,y,z axis -}
makeBasis : V3 -> V3 -> V3 -> M4x4
makeBasis = Native.MJS.m4x4makeBasis
