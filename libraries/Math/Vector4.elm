module Math.Vector4 where
{-| A high performance linear algebra library using native JS arrays. Geared
towards 3D graphics and use with `Graphics.WebGL`. All vectors are immutable.

# Create
@docs v4

# Get and Set
The set functions create a new copy of the vector, updating a single field.

@docs getX, getY, setX, setY

# Operations
@docs add, sub, negate, scale, dot, normalize, direction,
      length, lengthSquared, distance, distanceSquared

# Conversions
toTuple, fromTuple, toRecord, fromRecord
-}

import Native.Math.Vector4

data Vec4 = Vec4

{-| Creates a new 4-element vector with the given x, y, z, and w values. -}
v4 : Float -> Float -> Float -> Float -> Vec4
v4 = Native.Math.Vector4.v4

{-| Extract the x component of a vector. -}
getX : Vec4 -> Float
getX = Native.Math.Vector4.getX

{-| Extract the y component of a vector. -}
getY : Vec4 -> Float
getY = Native.Math.Vector4.getY

{-| Extract the z component of a vector. -}
getZ : Vec4 -> Float
getZ = Native.Math.Vector4.getZ

{-| Extract the w component of a vector. -}
getW : Vec4 -> Float
getW = Native.Math.Vector4.getW

{-| Update the x component of a vector, returning a new vector. -}
setX : Float -> Vec4 -> Vec4
setX = Native.Math.Vector4.setX

{-| Update the y component of a vector, returning a new vector. -}
setY : Float -> Vec4 -> Vec4
setY = Native.Math.Vector4.setY

{-| Update the z component of a vector, returning a new vector. -}
setZ : Float -> Vec4 -> Vec4
setZ = Native.Math.Vector4.setZ

{-| Update the w component of a vector, returning a new vector. -}
setW : Float -> Vec4 -> Vec4
setW = Native.Math.Vector4.setW

{-| Convert a vector to a tuple. -}
toTuple : Vec4 -> (Float,Float,Float,Float)
toTuple = Native.Math.Vector4.toTuple

{-| Convert a vector to a record. -}
toRecord : Vec4 -> { x:Float, y:Float, z:Float, w:Float }
toRecord = Native.Math.Vector4.toRecord

{-| Convert a tuple to a vector. -}
fromTuple : (Float,Float,Float,Float) -> Vec4
fromTuple = Native.Math.Vector4.fromTuple

{-| Convert a record to a vector. -}
fromRecord : { x:Float, y:Float, z:Float, w:Float } -> Vec4
fromRecord = Native.Math.Vector4.fromRecord

{-| Vector addition: a + b -}
add : Vec4 -> Vec4 -> Vec4
add = Native.Math.Vector4.add

{-| Vector subtraction: a - b -}
sub : Vec4 -> Vec4 -> Vec4
sub = Native.Math.Vector4.sub

{-| Vector negation: -a -}
negate : Vec4 -> Vec4
negate = Native.Math.Vector4.neg

{-| The normalized direction from a to b: (a - b) / |a - b| -}
direction : Vec4 -> Vec4 -> Vec4
direction = Native.Math.Vector4.direction

{-| The length of the given vector: |a| -}
length : Vec4 -> Float
length = Native.Math.Vector4.length

{-| The square of the length of the given vector: |a| * |a| -}
lengthSquared : Vec4 -> Float
lengthSquared = Native.Math.Vector4.lengthSquared

{-| The distance between two vectors. -}
distance : Vec4 -> Vec4 -> Float
distance = Native.Math.Vector4.distance

{-| The square of the distance between two vectors. -}
distanceSquared : Vec4 -> Vec4 -> Float
distanceSquared = Native.Math.Vector4.distanceSquared

{-| A unit vector with the same direction as the given vector: a / |a| -}
normalize : Vec4 -> Vec4
normalize = Native.Math.Vector4.normalize

{-| Multiply the vector by a scalar: a * k -}
scale : Vec4 -> Float -> Vec4
scale = Native.Math.Vector4.scale

{-| The dot product of a and b -}
dot : Vec4 -> Vec4 -> Float
dot = Native.Math.Vector4.dot

