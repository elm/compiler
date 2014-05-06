module Math.Matrix4 where

{-| A high performance linear algebra library using native JS arrays. Geared
towards 3D graphics and use with `Graphics.WebGL`. All matrices are immutable.

This library uses the convention that the prefix `make` is creating a new
array, whereas without the prefix, you are applying some transform to an
existing matrix.

# Create

@docs identity

# Operations

@docs inverseOrthonormal, mul, mulAffine, mulVec3, transpose, makeBasis

# Projections

@docs makeFrustrum, makePerspective, makeOrtho, makeOrtho2D, makeLookAt

# Apply Transformations

@docs rotate, scale, scale3, translate, translate3

# Create Transformations

@docs makeRotate, makeScale, makeScale3, makeTranslate, makeTranslate3

-}

import Native.MJS
import Math.Vector3 (Vec3)

data Mat4 = Mat4

{-| Multiply a vector by a 4x4 matrix: m * v
-}
mulVec3 : Mat4 -> Vec3 -> Vec3
mulVec3 = Native.MJS.v3mul4x4

{-| A matrix with all 0s, except 1s on the diagonal.
-}
identity : Mat4
identity = Native.MJS.m4x4identity

{-| Computes the inverse of the given matrix m, assuming that the matrix is
orthonormal.
-}
inverseOrthonormal : Mat4 -> Mat4
inverseOrthonormal = Native.MJS.m4x4inverseOrthonormal

{-| Creates a matrix for a projection frustum with the given parameters.

Parameters: 

 * left - the left coordinate of the frustum
 * right- the right coordinate of the frustum
 * bottom - the bottom coordinate of the frustum
 * top - the top coordinate of the frustum
 * znear - the near z distance of the frustum 
 * zfar - the far z distance of the frustum
-}
makeFrustrum : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Mat4
makeFrustrum = Native.MJS.m4x4makeFrustrum

{-| Creates a matrix for a perspective projection with the given parameters.

Parameters:

 * fovy - field of view in the y axis, in degrees
 * aspect - aspect ratio
 * znear - the near z distance of the projection
 * zfar - the far z distance of the projection
-}
makePerspective : Float -> Float -> Float -> Float -> Mat4
makePerspective = Native.MJS.m4x4makePerspective

{-|
Creates a matrix for an orthogonal frustum projection with the given parameters.

Parameters:

 * left - the left coordinate of the frustum
 * right- the right coordinate of the frustum
 * bottom - the bottom coordinate of the frustum 
 * top - the top coordinate of the frustum
 * znear - the near z distance of the frustum
 * zfar - the far z distance of the frustum
-}
makeOrtho : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Mat4
makeOrtho = Native.MJS.m4x4makeOrtho

{-| Creates a matrix for a 2D orthogonal frustum projection with the given
parameters. `znear` and `zfar` are assumed to be -1 and 1, respectively.

Parameters:

 * left - the left coordinate of the frustum
 * right- the right coordinate of the frustum
 * bottom - the bottom coordinate of the frustum
 * top - the top coordinate of the frustum
-}
makeOrtho2D : Float -> Float -> Float -> Float -> Mat4
makeOrtho2D = Native.MJS.m4x4makeOrtho2D

{-| Matrix multiplcation: a * b
-}
mul : Mat4 -> Mat4 -> Mat4
mul = Native.MJS.m4x4mul

{-| Matrix multiplication, assuming a and b are affine: a * b
-}
mulAffine : Mat4 -> Mat4 -> Mat4
mulAffine = Native.MJS.m4x4mulAffine

{-| Creates a transformation matrix for rotation in radians about the
3-element vector axis.
-}
makeRotate : Float -> Vec3 -> Mat4
makeRotate = Native.MJS.m4x4makeRotate

{-| Concatenates a rotation in radians about an axis to the given matrix.
-}
rotate : Float -> Vec3 -> Mat4 -> Mat4
rotate = Native.MJS.m4x4rotate

{-| Creates a transformation matrix for scaling by 3 scalar values, one for
each of the x, y, and z directions.
-}
makeScale3 : Float -> Float -> Float -> Mat4
makeScale3 = Native.MJS.m4x4makeScale3

{-| Creates a transformation matrix for scaling each of the x, y, and z axes by
the amount given in the corresponding element of the 3-element vector.
-}
makeScale : Vec3 -> Mat4
makeScale = Native.MJS.m4x4makeScale

{-| Concatenates a scaling to the given matrix.
-}
scale3 : Float -> Float -> Float -> Mat4 -> Mat4
scale3 = Native.MJS.m4x4scale3

{-| Concatenates a scaling to the given matrix.
-}
scale : Vec3 -> Mat4 -> Mat4
scale = Native.MJS.m4x4scale

{-|
Creates a transformation matrix for translating by 3 scalar values, one for
each of the x, y, and z directions.
-}
makeTranslate3 : Float -> Float -> Float -> Mat4
makeTranslate3 = Native.MJS.m4x4makeTranslate3

{-| Creates a transformation matrix for translating each of the x, y, and z
axes by the amount given in the corresponding element of the 3-element vector.
-}
makeTranslate : Vec3 -> Mat4
makeTranslate = Native.MJS.m4x4makeTranslate

{-|
Concatenates a translation to the given matrix.
-}
translate3 : Float -> Float -> Float -> Mat4 -> Mat4
translate3 = Native.MJS.m4x4translate3

{-|
Concatenates a translation to the given matrix.
-}
translate : Vec3 -> Mat4 -> Mat4
translate = Native.MJS.m4x4translate

{-|
Creates a transformation matrix for a camera.

Parameters:

 * eye - The location of the camera
 * center - The location of the focused object
 * up - The "up" direction according to the camera
-}
makeLookAt : Vec3 -> Vec3 -> Vec3 -> Mat4
makeLookAt = Native.MJS.m4x4makeLookAt

{-| "Flip" the matrix across the diagonal by swapping row index and column
index.
-}
transpose : Mat4 -> Mat4
transpose = Native.MJS.m4x4transpose

{-| Creates a transform from a basis consisting of 3 linearly independent vectors.
-}
makeBasis : Vec3 -> Vec3 -> Vec3 -> Mat4
makeBasis = Native.MJS.m4x4makeBasis
