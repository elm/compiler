{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls #-}
module AST.Utils.Shader
  ( Source
  , Types(..)
  , Type(..)
  , fromChars
  , toJsStringBuilder
  )
  where


import Control.Monad (liftM)
import Data.Binary (Binary, get, put)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.Map as Map
import qualified Data.Name as Name



-- SOURCE


newtype Source =
  Source BS.ByteString



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
  | Bool
  | V2
  | V3
  | V4
  | M4
  | Texture



-- TO BUILDER


toJsStringBuilder :: Source -> B.Builder
toJsStringBuilder (Source src) =
  B.byteString src



-- FROM CHARS


fromChars :: [Char] -> Source
fromChars chars =
  Source (BS_UTF8.fromString (escape chars))


escape :: [Char] -> [Char]
escape chars =
  case chars of
    [] ->
      []

    c:cs
      | c == '\r' -> escape cs
      | c == '\n' -> '\\' : 'n'  : escape cs
      | c == '\"' -> '\\' : '"'  : escape cs
      | c == '\'' -> '\\' : '\'' : escape cs
      | c == '\\' -> '\\' : '\\' : escape cs
      | otherwise -> c : escape cs



-- BINARY


instance Binary Source where
  get = liftM Source get
  put (Source a) = put a
