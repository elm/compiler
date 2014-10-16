module Transform2D ( Transform2D
                   , identity, matrix, multiply
                   , rotation, translation
                   , scale, scaleX, scaleY
                   ) where
{-| A library for performing [2D matrix transformations][affine].
It is used primarily with the `groupTransform` function from `Graphics.Collage` and
allows you to do things like rotation, scaling, translation, shearing, and reflection.

Note that all the matrices in this library are 3x3 matrices of homogeneous
coordinates, used for [affine transformations][affine]. Since the bottom row as
always `0 0 1` in these matrices, it is omitted in the diagrams below.

 [affine]: http://en.wikipedia.org/wiki/Transformation_matrix#Affine_transformations

# Transforms
@docs identity, matrix, rotation, translation, scale, scaleX, scaleY

# Multiplication
@docs multiply
-}

import Native.Transform2D

type Transform2D = Transform2D

{-| Create an identity transform. Transforming by the identity does
not change anything, but it can come in handy as a default or
base case.

          / 1 0 0 \
          \ 0 1 0 /
-}
identity : Transform2D
identity = Native.Transform2D.identity

{-| Create a transformation matrix. This lets you create transforms
such as scales, shears, reflections, and translations.

      matrix a b c d x y

          / a b x \
          \ c d y /

Note that `x` and `y` are the translation values.
-}
matrix : Float -> Float -> Float -> Float -> Float -> Float -> Transform2D
matrix = Native.Transform2D.matrix

{-| Create a [rotation matrix](http://en.wikipedia.org/wiki/Rotation_matrix).
Given an angle t, it creates a counterclockwise rotation matrix:

      rotation t

          / cos t  -sin t  0 \
          \ sin t   cos t  0 /
-}
rotation : Float -> Transform2D
rotation = Native.Transform2D.rotation

{-| Create a transformation matrix for translation.

      translation x y

          / 1 0 x \
          \ 0 1 y /
-}
translation : Float -> Float -> Transform2D
translation x y = matrix 1 0 0 1 x y

{-| Creates a transformation matrix for scaling by a all directions.

      scale s

        / s 0 0 \
        \ 0 s 0 /
-}
scale : Float -> Transform2D
scale s = matrix s 0 0 s 0 0

{-| Create a transformation for horizontal scaling. -}
scaleX : Float -> Transform2D
scaleX x = matrix x 0 0 1 0 0

{-| Create a transformation for vertical scaling. -}
scaleY : Float -> Transform2D
scaleY y = matrix 1 0 0 y 0 0

{-| Multiply two transforms together.

       multiply m n

          / ma mb mx \     / na nb nx \
          | mc md my |  .  | nc nd ny |
          \  0  0  1 /     \  0  0  1 /
-}
multiply : Transform2D -> Transform2D -> Transform2D
multiply = Native.Transform2D.multiply
