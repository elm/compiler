module Transform2D where
{-| A library for performing [2D matrix transformations](http://en.wikipedia.org/wiki/Transformation_matrix#Affine_transformations).
It is used primarily with the `groupTransform` function from `Graphics.Collage` and
allows you to do things like rotation, scaling, translation, shearing, and reflection.

Note that all the matrices in this library are 3x3 matrices of homogeneous
coordinates, used for affine transformations. Since the third row as always 0 0
1, we omit this below.

# Basic Transforms
@docs identity, matrix, rotation

# Multiplication
@docs multiply
-}

import Native.Transform2D

data Transform2D = Transform2D

{-| Create an identity transform. Transforming by the identity does
not change anything, but it can come in handy as a default or
base case.

          / 1 0 0 \
          \ 0 1 0 /
-}
identity : Transform2D
identity = Native.Transform2D.identity

{-| Creates a transformation matrix. This lets you create transforms
such as scales, shears, reflections, and translations.

      matrix a b c d dx dy

          / a b dx \
          \ c d dy /

Note that `dx` and `dy` are the translation values.
-}
matrix : Float -> Float -> Float -> Float -> Float -> Float -> Transform2D
matrix = Native.Transform2D.matrix

{-| Creates a [rotation matrix](http://en.wikipedia.org/wiki/Rotation_matrix).
Given an angle t, it creates a counterclockwise rotation matrix:

          / cos t  -sin t  0 \
          \ sin t   cos t  0 /
-}
rotation : Float -> Transform2D
rotation = Native.Transform2D.rotation

{-| Creates a transformation matrix for translation:

    translation x y

          / 1 0 x \
          | 0 1 y |
          \ 0 0 1 /
-}
translation : Float -> Float -> Transform2D
translation x y = matrix 1 0 0 1 x y

{-| Creates a transformation matrix for scaling by a all directions:

    scale s

        / s 0 0 \
        \ 0 s 0 /
-}
scale : Float -> Transform2D
scale s = matrix s 0 0 s 0 0

{-| Creates a transformation for horizontal scaling -}
scaleX : Float -> Transform2D
scaleX x = matrix x 0 0 1 0 0

{-| Creates a transformation for vertical scaling -}
scaleY : Float -> Transform2D
scaleY y = matrix y 0 0 1 0 0

{-| Multiplies two transforms together:

      multiply a b

          / a11 a12 \  .  / b11 b12 \
          \ a21 a22 /     \ b21 b22 /
-}
multiply : Transform2D -> Transform2D -> Transform2D
multiply = Native.Transform2D.multiply
