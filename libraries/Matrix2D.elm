
module Graphics.Matrix where

import Native.Graphics.Matrix as M

identity  : Matrix

transform : Float -> Float -> Float -> Float -> Float -> Float -> Matrix -> Matrix
rotate    : Float -> Matrix -> Matrix
move      : Float -> Float -> Matrix -> Matrix
scale     : Float -> Matrix -> Matrix
scaleX    : Float -> Matrix -> Matrix
scaleY    : Float -> Matrix -> Matrix
reflectX  : Matrix -> Matrix
reflectY  : Matrix -> Matrix

multiply  : Matrix -> Matrix -> Matrix
matrix    : Float -> Float -> Float -> Float -> Float -> Float -> Matrix
