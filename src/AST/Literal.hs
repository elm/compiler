{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Literal where

import qualified Data.Map as Map
import Data.Text (Text)

import qualified AST.Variable as Var



-- LITERALS


data Literal
    = IntNum Int
    | FloatNum Double
    | Chr Char
    | Str Text
    | Boolean Bool
    deriving (Eq, Ord)


toString :: Literal -> String
toString literal =
    case literal of
      IntNum n -> show n
      FloatNum n -> show n
      Chr c -> show c
      Str s -> show s
      Boolean bool -> show bool



-- WebGL TYPES


data GLType
  = Int
  | Float
  | V2
  | V3
  | V4
  | M4
  | Texture
  deriving (Eq)


glTypeToVar :: GLType -> Var.Canonical
glTypeToVar glTipe =
  case glTipe of
    V2 -> Var.inLinearAlgebra "Math.Vector2" "Vec2"
    V3 -> Var.inLinearAlgebra "Math.Vector3" "Vec3"
    V4 -> Var.inLinearAlgebra "Math.Vector4" "Vec4"
    M4 -> Var.inLinearAlgebra "Math.Matrix4" "Mat4"
    Int -> Var.builtin "Int"
    Float -> Var.builtin "Float"
    Texture -> Var.inWebGL "WebGL" "Texture"


data Shader =
  Shader
    { attribute :: Map.Map Text GLType
    , uniform :: Map.Map Text GLType
    , varying :: Map.Map Text GLType
    }
    deriving (Eq)
