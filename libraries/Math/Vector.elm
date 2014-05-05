module Math.Vector where

{-| A high performance linear algebra library using native JS arrays. Geared
towards 3D graphics and use with `Graphics.WebGL`.

All vectors are immutable.

# Create
@docs v3, i, j, k

# Get and Set
The set functions create a new copy of the vector, updating a single field.

@docs getX, getY, getZ, setX, setY, setZ, toTuple, toRecord

# Operations
@docs add, sub, neg, direction, length, lengthSquared, normalize, scale, dot, cross

-}

import Native.MJS

data Vec2 = Vec2
data Vec3 = Vec3
data Vec4 = Vec4

{-| Creates a new 3-element vector with the given values.
-}
v3 : Float -> Float -> Float -> Vec3
v3 = Native.MJS.v3

{-| The unit vector &icirc; which points in the x direction: `v3 1 0 0` -}
i : Vec3
i = Native.MJS.v3 1 0 0

{-| The unit vector &jcirc; which points in the y direction: `v3 0 1 0` -}
j : Vec3
j = Native.MJS.v3 0 1 0

{-| The unit vector &kcirc; which points in the z direction: `v3 0 0 1` -}
k : Vec3
k = Native.MJS.v3 0 0 1

{-| Extract the x component of a vector. -}
getX : Vec3 -> Float
getX = Native.MJS.getX

{-| Extract the y component of a vector. -}
getY : Vec3 -> Float
getY = Native.MJS.getY

{-| Extract the z component of a vector. -}
getZ : Vec3 -> Float
getZ = Native.MJS.getZ

{-| Update the x component of a vector. -}
setX : Vec3 -> Float
setX = Native.MJS.setX

{-| Update the y component of a vector. -}
setY : Vec3 -> Float
setY = Native.MJS.setY

{-| Update the z component of a vector. -}
setZ : Vec3 -> Float
setZ = Native.MJS.setZ

{-| Convert a vector to a tuple.
-}
toTuple : Vec3 -> (Float,Float,Float)
toTuple = Native.MJS.toTuple3

{-| Convert a vector to a record.
-}
toRecord : Vec3 -> { x:Float, y:Float, z:Float }
toRecord = Native.MJS.toRecord3

{-| Vector addition: a + b
-}
add : Vec3 -> Vec3 -> Vec3
add = Native.MJS.v3add

{-| Vector subtraction: a - b
-}
sub : Vec3 -> Vec3 -> Vec3
sub = Native.MJS.v3sub

{-| Vector negation: -a
-}
neg : Vec3 -> Vec3
neg = Native.MJS.v3neg

{-| The normalized direction from a to b: (a - b) / |a - b|
-}
direction : Vec3 -> Vec3 -> Vec3
direction = Native.MJS.v3direction

{-| The length of the given vector: |a|
-}
length : Vec3 -> Float
length = Native.MJS.v3length

{-| The square of the length of the given vector: |a| * |a|
-}
lengthSquared : Vec3 -> Float
lengthSquared = Native.MJS.v3lengthSquared

{-| A unit vector with the same direction as the given vector: a / |a|
-}
normalize : Vec3 -> Vec3
normalize = Native.MJS.v3normalize

{-| Multiply the vector by a scalar: a * k
-}
scale : Vec3 -> Float -> Vec3
scale = Native.MJS.v3scale

{-| The dot product of a and b
-}
dot : Vec3 -> Vec3 -> Float
dot = Native.MJS.v3dot

{-| The cross product of a and b
-}
cross : Vec3 -> Vec3 -> Vec3
cross = Native.MJS.v3cross
