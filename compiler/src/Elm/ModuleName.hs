{-# LANGUAGE OverloadedStrings #-}
module Elm.ModuleName
  ( Raw
  , toChars
  , toString
  , toSlashPath
  , toHyphenPath
  , fromHyphenPath
  , encode
  , decoder
  , Canonical(..)
  , canonicalIsKernel
  , basics, char, string
  , maybe, result, list, array, dict, tuple
  , platform, cmd, sub
  , debug, bitwise
  , virtualDom
  , jsonDecode, jsonEncode
  , webgl, texture, vector2, vector3, vector4, matrix4
  )
  where


import Control.Monad (liftM2)
import Data.Binary (Binary(..))
import qualified Data.Char as Char
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8
import Prelude hiding (maybe)
import qualified System.FilePath as FP

import qualified Elm.Package as Pkg
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode



-- RAW


type Raw = Name.Name


toChars :: Raw -> String
toChars =
  Name.toChars


toString :: Raw -> Utf8.String
toString =
  Name.toUtf8


toSlashPath :: Raw -> FilePath
toSlashPath name =
  map (\c -> if c == '.' then FP.pathSeparator else c) (Name.toChars name)


toHyphenPath :: Raw -> FilePath
toHyphenPath name =
  map (\c -> if c == '.' then '-' else c) (Name.toChars name)


fromHyphenPath :: Utf8.String -> Maybe Raw
fromHyphenPath str =
  let
    chunks =
      Utf8.split 0x2D {- - -} str
  in
  if all isGoodChunk chunks
    then Just (Name.fromUtf8 (Utf8.join 0x2E {- . -} chunks))
    else Nothing



-- JSON


encode :: Raw -> Encode.Value
encode =
  Encode.name


decoder :: Decode.Decoder Utf8.String Raw
decoder =
  do  str <- Decode.string
      if all isGoodChunk (Utf8.split 0x2E {- . -} str)
        then return (Name.fromUtf8 str)
        else Decode.failure str


isGoodChunk :: Utf8.String -> Bool
isGoodChunk chunk =
  Utf8.startsWithChar Char.isUpper chunk
  && Utf8.all (\c -> Char.isAlphaNum c || c == '_') chunk



-- CANONICAL


data Canonical =
  Canonical
    { _package :: !Pkg.Name
    , _module :: !Name.Name
    }
    deriving (Ord)


canonicalIsKernel :: Canonical -> Bool
canonicalIsKernel (Canonical _ name) =
  Name.isKernel name



-- INSTANCES


instance Eq Canonical where
  (==) (Canonical pkg1 name1) (Canonical pkg2 name2) =
    name1 == name2 && pkg1 == pkg2


instance Binary Canonical where
  put (Canonical a b) = put a >> put b
  get = liftM2 Canonical get get



-- CORE


{-# NOINLINE basics #-}
basics :: Canonical
basics = Canonical Pkg.core Name.basics


{-# NOINLINE char #-}
char :: Canonical
char = Canonical Pkg.core Name.char


{-# NOINLINE string #-}
string :: Canonical
string = Canonical Pkg.core Name.string


{-# NOINLINE maybe #-}
maybe :: Canonical
maybe = Canonical Pkg.core Name.maybe


{-# NOINLINE result #-}
result :: Canonical
result = Canonical Pkg.core Name.result


{-# NOINLINE list #-}
list :: Canonical
list = Canonical Pkg.core Name.list


{-# NOINLINE array #-}
array :: Canonical
array = Canonical Pkg.core Name.array


{-# NOINLINE dict #-}
dict :: Canonical
dict = Canonical Pkg.core Name.dict


{-# NOINLINE tuple #-}
tuple :: Canonical
tuple = Canonical Pkg.core Name.tuple


{-# NOINLINE platform #-}
platform :: Canonical
platform = Canonical Pkg.core Name.platform


{-# NOINLINE cmd #-}
cmd :: Canonical
cmd = Canonical Pkg.core "Platform.Cmd"


{-# NOINLINE sub #-}
sub :: Canonical
sub = Canonical Pkg.core "Platform.Sub"


{-# NOINLINE debug #-}
debug :: Canonical
debug = Canonical Pkg.core Name.debug


{-# NOINLINE bitwise #-}
bitwise :: Canonical
bitwise = Canonical Pkg.core Name.bitwise



-- HTML


{-# NOINLINE virtualDom #-}
virtualDom :: Canonical
virtualDom = Canonical Pkg.virtualDom Name.virtualDom



-- JSON


{-# NOINLINE jsonDecode #-}
jsonDecode :: Canonical
jsonDecode = Canonical Pkg.json "Json.Decode"


{-# NOINLINE jsonEncode #-}
jsonEncode :: Canonical
jsonEncode = Canonical Pkg.json "Json.Encode"



-- WEBGL


{-# NOINLINE webgl #-}
webgl :: Canonical
webgl = Canonical Pkg.webgl "WebGL"


{-# NOINLINE texture #-}
texture :: Canonical
texture = Canonical Pkg.webgl "WebGL.Texture"


{-# NOINLINE vector2 #-}
vector2 :: Canonical
vector2 = Canonical Pkg.linearAlgebra "Math.Vector2"


{-# NOINLINE vector3 #-}
vector3 :: Canonical
vector3 = Canonical Pkg.linearAlgebra "Math.Vector3"


{-# NOINLINE vector4 #-}
vector4 :: Canonical
vector4 = Canonical Pkg.linearAlgebra "Math.Vector4"


{-# NOINLINE matrix4 #-}
matrix4 :: Canonical
matrix4 = Canonical Pkg.linearAlgebra "Math.Matrix4"
