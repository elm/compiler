module Math.Vector2 where
{-| A high performance linear algebra library using native JS arrays. Geared
towards 3D graphics and use with `Graphics.WebGL`. All vectors are immutable.

# Create
@docs v2

# Get and Set
The set functions create a new copy of the vector, updating a single field.

@docs getX, getY, setX, setY

# Operations
@docs add, sub, negate, scale, dot, normalize, direction,
      length, lengthSquared, distance, distanceSquared

# Conversions
toTuple, fromTuple, toRecord, fromRecord
-}

import Native.Math.Vector2

data Vec2 = Vec2

{-| Creates a new 2-element vector with the given values. -}
v2 : Float -> Float -> Vec2
v2 = Native.Math.Vector2.v2

{-| Extract the x component of a vector. -}
getX : Vec2 -> Float
getX = Native.Math.Vector2.getX

{-| Extract the y component of a vector. -}
getY : Vec2 -> Float
getY = Native.Math.Vector2.getY

{-| Update the x component of a vector, returning a new vector. -}
setX : Float -> Vec2 -> Vec2
setX = Native.Math.Vector2.setX

{-| Update the y component of a vector, returning a new vector. -}
setY : Float -> Vec2 -> Vec2
setY = Native.Math.Vector2.setY

{-| Convert a vector to a tuple. -}
toTuple : Vec2 -> (Float,Float)
toTuple = Native.Math.Vector2.toTuple

{-| Convert a vector to a record. -}
toRecord : Vec2 -> { x:Float, y:Float }
toRecord = Native.Math.Vector2.toRecord

{-| Convert a tuple to a vector. -}
fromTuple : (Float,Float) -> Vec2
fromTuple = Native.Math.Vector2.fromTuple

{-| Convert a record to a vector. -}
fromRecord : { x:Float, y:Float } -> Vec2
fromRecord = Native.Math.Vector2.fromRecord

{-| Vector addition: a + b -}
add : Vec2 -> Vec2 -> Vec2
add = Native.Math.Vector2.add

{-| Vector subtraction: a - b -}
sub : Vec2 -> Vec2 -> Vec2
sub = Native.Math.Vector2.sub

{-| Vector negation: -a -}
negate : Vec2 -> Vec2
negate = Native.Math.Vector2.neg

{-| The normalized direction from a to b: (a - b) / |a - b| -}
direction : Vec2 -> Vec2 -> Vec2
direction = Native.Math.Vector2.direction

{-| The length of the given vector: |a| -}
length : Vec2 -> Float
length = Native.Math.Vector2.length

{-| The square of the length of the given vector: |a| * |a| -}
lengthSquared : Vec2 -> Float
lengthSquared = Native.Math.Vector2.lengthSquared

{-| The distance between two vectors. -}
distance : Vec2 -> Vec2 -> Float
distance = Native.Math.Vector2.distance

{-| The square of the distance between two vectors. -}
distanceSquared : Vec2 -> Vec2 -> Float
distanceSquared = Native.Math.Vector2.distanceSquared

{-| A unit vector with the same direction as the given vector: a / |a| -}
normalize : Vec2 -> Vec2
normalize = Native.Math.Vector2.normalize

{-| Multiply the vector by a scalar: a * k -}
scale : Vec2 -> Float -> Vec2
scale = Native.Math.Vector2.scale

{-| The dot product of a and b -}
dot : Vec2 -> Vec2 -> Float
dot = Native.Math.Vector2.dot

