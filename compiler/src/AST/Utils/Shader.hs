{-# LANGUAGE EmptyDataDecls #-}
module AST.Utils.Shader
  ( Source
  , Types(..)
  , Type(..)
  , fromChars
  , toJsStringBuilder
  --
  , eSource, dSource
  )
  where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.Map as Map
import qualified Data.Name as Name

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E



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


eSource :: Source -> E.Builder
eSource (Source src) =
  E.byteString64 src


dSource :: D.Decoder Source
dSource =
  Source <$> D.byteString64

