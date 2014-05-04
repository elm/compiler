module MJS where

{-| MJS

A high performance linear algebra library using
native JS arrays. Geared towards 3D graphics
and use with Graphics.WebGL.

Both vectors and matrices are immutable.

# Constructing and Destructing Vectors

@docs v3, toTuple3

# Vector Math

@docs add, sub, neg, direction, length, lengthSquared, normalize, scale

# Vector Operations

@docs dot, cross, mul4x4

Matrix operations

@docs identity, inverseOrthonormal, mul, mulAffine, transpose, makeBasis

Matrix projections

@docs makeFrustrum, makePerspective, makeOrtho, makeOrtho2D, makeLookAt

Matrix transforms

@docs makeRotate, rotate, makeScale3, makeScale, scaleM3, scaleM, makeTranslate3, makeTranslate, translate3, translate

-}

import Native.MJS

data V2 = V2
data V3 = V3
data V4 = V4

{-|
Creates a new 3-element vector with the given values
-}
v3 : Float -> Float -> Float -> V3
v3 = Native.MJS.v3

{-|
Convert to a native tuple
-}
toTuple3 : V3 -> (Float,Float,Float)
toTuple3 = Native.MJS.toTuple3

{-|
Vector addition: a + b
-}
add : V3 -> V3 -> V3
add = Native.MJS.v3add

{-|
Vector subtraction: a - b
-}
sub : V3 -> V3 -> V3
sub = Native.MJS.v3sub

{-|
Vector negation: -a
-}
neg : V3 -> V3
neg = Native.MJS.v3neg

{-|
The normalized direction from a to b: (a - b) / |a - b|
-}
direction : V3 -> V3 -> V3
direction = Native.MJS.v3direction

{-|
The length of the given vector: |a|
-}
length : V3 -> Float
length = Native.MJS.v3length

{-|
The square of the length of the given vector: |a|*|a|
-}
lengthSquared : V3 -> Float
lengthSquared = Native.MJS.v3lengthSquared

{-|
A unit vector with the same direction as the given vector: a / |a|
-}
normalize : V3 -> V3
normalize = Native.MJS.v3normalize

{-|
Multiply the vector by a scalar: a * k
-}
scale : V3 -> Float -> V3
scale = Native.MJS.v3scale

{-|
The dot product of a and b
-}
dot : V3 -> V3 -> Float
dot = Native.MJS.v3dot

{-|
The cross product of a and b
-}
cross : V3 -> V3 -> V3
cross = Native.MJS.v3cross

{-|
Methods for working with 4x4 matrices.
-}
data M4x4 = M4x4

{-|
Multiple the vector by the 4x4 matrix: m * v
-}
mul4x4 : M4x4 -> V3 -> V3
mul4x4 = Native.MJS.v3mul4x4

{-|
A matrix with all 0s, except 1s on the diagonal
-}
identity : M4x4
identity = Native.MJS.m4x4identity

{-|
Computes the inverse of the given matrix m, assuming that
the matrix is orthonormal
-}
inverseOrthonormal : M4x4 -> M4x4
inverseOrthonormal = Native.MJS.m4x4inverseOrthonormal

{-|
Creates a matrix for a projection frustum with the given parameters.

Parameters: 

  left - the left coordinate of the frustum
  right- the right coordinate of the frustum
  bottom - the bottom coordinate of the frustum
  top - the top coordinate of the frustum
  znear - the near z distance of the frustum 
  zfar - the far z distance of the frustum
-}
makeFrustrum : Float -> Float -> Float -> Float -> Float -> Float -> Float -> M4x4
makeFrustrum = Native.MJS.m4x4makeFrustrum

{-|
Creates a matrix for a perspective projection with the given parameters.

Parameters: 

  fovy - field of view in the y axis, in degrees
  aspect - aspect ratio
  znear - the near z distance of the projection
  zfar - the far z distance of the projection
-}
makePerspective : Float -> Float -> Float -> Float -> M4x4
makePerspective = Native.MJS.m4x4makePerspective

{-|
Creates a matrix for an orthogonal frustum projection with the given parameters.

Parameters:

  left - the left coordinate of the frustum
  right- the right coordinate of the frustum
  bottom - the bottom coordinate of the frustum 
  top - the top coordinate of the frustum
  znear - the near z distance of the frustum
  zfar - the far z distance of the frustum
-}
makeOrtho : Float -> Float -> Float -> Float -> Float -> Float -> Float -> M4x4
makeOrtho = Native.MJS.m4x4makeOrtho

{-|
Creates a matrix for a 2D orthogonal frustum projection with the given parameters.
znear and zfar are assumed to be -1 and 1, respectively.

Parameters:

  left - the left coordinate of the frustum
  right- the right coordinate of the frustum
  bottom - the bottom coordinate of the frustum
  top - the top coordinate of the frustum
-}
makeOrtho2D : Float -> Float -> Float -> Float -> M4x4
makeOrtho2D = Native.MJS.m4x4makeOrtho2D

{-|
Matrix multiplcation: a * b
-}
mul : M4x4 -> M4x4 -> M4x4
mul = Native.MJS.m4x4mul

{-|
Matrix multiplication, assuming a and b are affine: a * b
-}
mulAffine : M4x4 -> M4x4 -> M4x4
mulAffine = Native.MJS.m4x4mulAffine

{-|
Creates a transformation matrix for rotation by angle radians about the 3-element vector axis.
-}
makeRotate : Float -> V3 -> M4x4
makeRotate = Native.MJS.m4x4makeRotate

{-|
Concatenates a rotation of angle radians about the axis to the given matrix.
-}
rotate : Float -> V3 -> M4x4 -> M4x4
rotate = Native.MJS.m4x4rotate

{-|
Creates a transformation matrix for scaling by 3 scalar values, one for
each of the x, y, and z directions.
-}
makeScale3 : Float -> Float -> Float -> M4x4
makeScale3 = Native.MJS.m4x4makeScale3

{-|
Creates a transformation matrix for scaling each of the x, y, and z axes by the amount
given in the corresponding element of the 3-element vector.
-}
makeScale : V3 -> M4x4
makeScale = Native.MJS.m4x4makeScale

{-|
Concatenates a scaling to the given matrix.
-}
scaleM3 : Float -> Float -> Float -> M4x4 -> M4x4
scaleM3 = Native.MJS.m4x4scale3

{-|
Concatenates a scaling to the given matrix.
-}
scaleM : V3 -> M4x4 -> M4x4
scaleM = Native.MJS.m4x4scale

{-|
Creates a transformation matrix for translating by 3 scalar values, one for
each of the x, y, and z directions.
-}
makeTranslate3 : Float -> Float -> Float -> M4x4
makeTranslate3 = Native.MJS.m4x4makeTranslate3

{-|
Creates a transformation matrix for translating each of the x, y, and z axes by the amount
given in the corresponding element of the 3-element vector.
-}
makeTranslate : V3 -> M4x4
makeTranslate = Native.MJS.m4x4makeTranslate

{-|
Concatenates a translation to the given matrix.
-}
translate3 : Float -> Float -> Float -> M4x4 -> M4x4
translate3 = Native.MJS.m4x4translate3

{-|
Concatenates a translation to the given matrix.
-}
translate : V3 -> M4x4 -> M4x4
translate = Native.MJS.m4x4translate

{-|
Creates a transformation matrix for a camera.

Parameters:
  
 * eye - The location of the camera
 * center - The location of the focused object
 * up - The "up" direction according to the camera
-}
makeLookAt : V3 -> V3 -> V3 -> M4x4
makeLookAt = Native.MJS.m4x4makeLookAt

{-|
"Flip" the matrix across the diagonal by swapping row index and column index.
-}
transpose : M4x4 -> M4x4
transpose = Native.MJS.m4x4transpose

{-| 
Creates a transform from a basis consisting of 3 linearly independent vectors.
-}
makeBasis : V3 -> V3 -> V3 -> M4x4
makeBasis = Native.MJS.m4x4makeBasis
