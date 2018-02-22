{-# OPTIONS_GHC -Wall #-}
module AST.Utils.Shader
  ( Shader(..)
  , Type(..)
  )
  where


import qualified Data.Map as Map
import qualified Elm.Name as N



-- SHADERS


data Shader =
  Shader
    { _attribute :: Map.Map N.Name Type
    , _uniform :: Map.Map N.Name Type
    , _varying :: Map.Map N.Name Type
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
