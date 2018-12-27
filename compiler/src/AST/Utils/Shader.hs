{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls #-}
module AST.Utils.Shader
  ( Source
  , Types(..)
  , Type(..)
  )
  where


import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8



-- SOURCE


type Source = Utf8.VeryLong SOURCE

data SOURCE



-- TYPES


data Types =
  Types
    { _attribute :: Map.Map Name.Name Type
    , _uniform :: Map.Map Name.Name Type
    , _varying :: Map.Map Name.Name Type
    }


data Type
  = Int
  | Float
  | V2
  | V3
  | V4
  | M4
  | Texture
