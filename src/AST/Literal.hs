{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Literal where

import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, fromString)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)

import qualified AST.Variable as Var



-- LITERALS


data Literal
  = Chr Char
  | Str Text
  | IntNum Int
  | FloatNum Double
  | Boolean Bool
  deriving (Eq, Ord)


toBuilder :: Literal -> Builder
toBuilder literal =
  case literal of
    Chr c -> fromString (show c)
    Str s -> fromString (show s)
    IntNum n -> decimal n
    FloatNum n -> realFloat n
    Boolean bool -> if bool then "True" else "False"



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
