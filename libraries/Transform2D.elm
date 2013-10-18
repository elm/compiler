module Transform2D where
{-| A library for performing [2D matrix transformations](http://en.wikipedia.org/wiki/Transformation_matrix#Affine_transformations).
It is used primarily with the `groupTransform` function from `Graphics.Collage` and
allows you to do things like rotation, scaling, translation, shearing, and reflection.

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

          / 1 0 \
          \ 0 1 /
-}
identity : Transform2D
identity = Native.Transform2D.identity

{-| Creates a transformation matrix. This lets you create transforms
such as scales, shears, reflections, and translations.

      matrix a b c d dx dy

          / a b \
          \ c d /

And `dx` and `dy` are the translation values.
-}
matrix : Float -> Float -> Float -> Float -> Float -> Float -> Transform2D
matrix = Native.Transform2D.matrix

{-| Creates a [rotation matrix](http://en.wikipedia.org/wiki/Rotation_matrix).
Given an angle t, it creates a counterclockwise rotation matrix:

          / cos t  -sin t \
          \ sin t   cos t /
-}
rotation : Float -> Transform2D
rotation = Native.Transform2D.rotation

{-| Multiplies two transforms together:

      multiply a b

          / a11 a12 \  .  / b11 b12 \
          \ a21 a22 /     \ b21 b22 /
-}
multiply : Transform2D -> Transform2D -> Transform2D
multiply = Native.Transform2D.multiply