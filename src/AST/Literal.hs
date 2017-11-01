{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Literal
  ( Literal(..)
  , toBuilder
  , Shader(..)
  , GLType(..)
  )
  where


import Data.Binary
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
  deriving (Eq, Ord)


toBuilder :: Literal -> Builder
toBuilder literal =
  case literal of
    Chr c -> fromText ("'" <> c <> "'")
    Str s -> fromText ("\"" <> s <> "\"")
    IntNum n -> decimal n
    FloatNum n -> realFloat n



-- WebGL TYPES


data Shader =
  Shader
    { attribute :: Map.Map Text GLType
    , uniform :: Map.Map Text GLType
    , varying :: Map.Map Text GLType
    }
    deriving (Eq)


data GLType
  = Int
  | Float
  | V2
  | V3
  | V4
  | M4
  | Texture
  deriving (Eq)



-- BINARY


instance Binary Literal where
  put literal =
    case literal of
      Chr chr ->
        putWord8 0 >> put chr

      Str str ->
        putWord8 1 >> put str

      IntNum n ->
        putWord8 2 >> put n

      FloatNum n ->
        putWord8 3 >> put n

  get =
    do  word <- getWord8
        case word of
          0 -> Chr <$> get
          1 -> Str <$> get
          2 -> IntNum <$> get
          3 -> FloatNum <$> get
          _ -> error "bad binary for AST.Literal"
