{-# OPTIONS_GHC -Wall #-}
module AST.Utils.Shader
  ( Shader(..)
  , Type(..)
  )
  where


import qualified Data.Map as Map
import qualified Data.Name as Name



-- SHADERS


data Shader =
  Shader
    { _attribute :: Map.Map Name.Name Type
    , _uniform :: Map.Map Name.Name Type
    , _varying :: Map.Map Name.Name Type
    }



-- GL TYPES


data Type
  = Int
  | Float
  | V2
  | V3
  | V4
  | M4
  | Texture
