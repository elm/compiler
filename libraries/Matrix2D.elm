
module Matrix2D where

import Native.Matrix2D as Native

-- Create an identity matrix.
--
--           / 1 0 \\
--           \\ 0 1 /
identity : Matrix2D
identity = Native.identity

-- Creates an arbitrary matrix. This lets you create scales, shears, reflections,
-- translations, or any other 2D transform.
--
--       matrix a b c d dx dy
--
--           / a b \\
--           \\ c d /
--
-- And `dx` and `dy` are the translation values.
matrix : Float -> Float -> Float -> Float -> Float -> Float -> Matrix2D
matrix = Native.matrix

-- Creates a [rotation matrix](http://en.wikipedia.org/wiki/Rotation_matrix).
-- Given an angle t, it creates a counterclockwise rotation matrix:
--
--           / cos t  -sin t \\
--           \\ sin t   cos t /
rotation : Float -> Matrix2D
rotation = Native.rotation

-- Multiplies two matrices together:
--
--       multiply a b
--
--           / a11 a12 \\  .  / b11 b12 \\
--           \\ a21 a22 /     \\ b21 b22 /
multiply : Matrix2D -> Matrix2D -> Matrix2D
multiply = Native.multiply