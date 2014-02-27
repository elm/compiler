{-# OPTIONS_GHC -Wall #-}
module SourceSyntax.Literal where

import Data.Map (Map)
import SourceSyntax.PrettyPrint
import qualified Text.PrettyPrint as PP

data Literal = IntNum Int
             | FloatNum Double
             | Chr Char
             | Str String
             | Boolean Bool
             deriving (Eq, Ord, Show)

instance Pretty Literal where
  pretty literal =
    case literal of
      IntNum n -> PP.int n
      FloatNum n -> PP.double n
      Chr c -> PP.text . show $ c
      Str s -> PP.text . show $ s
      Boolean bool -> PP.text (show bool)

data GLTipe = Int | Float | V2 | V3 | V4 | M4 | Texture
  deriving (Show)

glTipeName :: GLTipe -> String
glTipeName Int = "Int"
glTipeName Float = "Float"
glTipeName V2 = "MJS.V2"
glTipeName V3 = "MJS.V3"
glTipeName V4 = "MJS.V4"
glTipeName M4 = "MJS.M4x4"
glTipeName Texture = "Graphics.WebGL.Texture"

data GLShaderTipe = GLShaderTipe
    { attribute :: Map String GLTipe
    , uniform :: Map String GLTipe
    , varying :: Map String GLTipe
    } deriving (Show)

