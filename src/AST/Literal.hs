{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Literal where

import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, fromText)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)



-- LITERALS


data Literal
  = Chr Text
  | Str Text
  | IntNum Int
  | FloatNum Double
  | Boolean Bool
  deriving (Eq, Ord)


toBuilder :: Literal -> Builder
toBuilder literal =
  case literal of
    Chr c -> fromText ("'" <> c <> "'")
    Str s -> fromText ("\"" <> s <> "\"")
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


glTypeToText :: GLType -> Text
glTypeToText glTipe =
  case glTipe of
    V2 -> "Math.Vector2.Vec2"
    V3 -> "Math.Vector3.Vec3"
    V4 -> "Math.Vector4.Vec4"
    M4 -> "Math.Matrix4.Mat4"
    Int -> "Int"
    Float -> "Float"
    Texture -> "WebGL.Texture"


data Shader =
  Shader
    { attribute :: Map.Map Text GLType
    , uniform :: Map.Map Text GLType
    , varying :: Map.Map Text GLType
    }
    deriving (Eq)
