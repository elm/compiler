{-# LANGUAGE BangPatterns, OverloadedStrings, UnboxedTuples #-}
module Elm.ModuleName
  ( Raw
  , toChars
  , toFilePath
  , toHyphenPath
  --
  , encode
  , decoder
  , parser
  --
  , Canonical(..)
  , basics, char, string
  , maybe, result, list, array, dict, tuple
  , platform, cmd, sub
  , debug
  , virtualDom
  , jsonDecode, jsonEncode
  , webgl, texture, vector2, vector3, vector4, matrix4
  )
  where


import Control.Monad (liftM2)
import Data.Binary (Binary(..))
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import Prelude hiding (maybe)
import qualified System.FilePath as FP

import qualified Elm.Package as Pkg
import qualified Json.Decode as D
import qualified Json.Encode as E
import qualified Parse.Variable as Var
import qualified Parse.Primitives as P
import Parse.Primitives (Row, Col)



-- RAW


type Raw = Name.Name


toChars :: Raw -> String
toChars =
  Name.toChars


toFilePath :: Raw -> FilePath
toFilePath name =
  map (\c -> if c == '.' then FP.pathSeparator else c) (Name.toChars name)


toHyphenPath :: Raw -> FilePath
toHyphenPath name =
  map (\c -> if c == '.' then '-' else c) (Name.toChars name)



-- JSON


encode :: Raw -> E.Value
encode =
  E.name


decoder :: D.Decoder (Row, Col) Raw
decoder =
  D.customString parser (,)



-- PARSER


parser :: P.Parser (Row, Col) Raw
parser =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr eerr ->
    let
      (# isGood, newPos, newCol #) = chompStart pos end col
    in
    if isGood && minusPtr newPos pos < 256 then
      let !newState = P.State src newPos end indent row newCol in
      cok (Utf8.fromPtr pos newPos) newState

    else if col == newCol then
      eerr row newCol (,)

    else
      cerr row newCol (,)


chompStart :: Ptr Word8 -> Ptr Word8 -> Col -> (# Bool, Ptr Word8, Col #)
chompStart pos end col =
  let
    !width = Var.getUpperWidth pos end
  in
  if width == 0 then
    (# False, pos, col #)
  else
    chompInner (plusPtr pos width) end (col + 1)


chompInner :: Ptr Word8 -> Ptr Word8 -> Col -> (# Bool, Ptr Word8, Col #)
chompInner pos end col =
  if pos >= end then
    (# True, pos, col #)
  else
    let
      !word = P.unsafeIndex pos
      !width = Var.getInnerWidthHelp pos end word
    in
    if width == 0 then
      if word == 0x2E {-.-} then
        chompStart (plusPtr pos 1) end (col + 1)
      else
        (# True, pos, col #)
    else
      chompInner (plusPtr pos width) end (col + 1)



-- CANONICAL


data Canonical =
  Canonical
    { _package :: !Pkg.Name
    , _module :: !Name.Name
    }



-- INSTANCES


instance Eq Canonical where
  (==) (Canonical pkg1 name1) (Canonical pkg2 name2) =
    name1 == name2 && pkg1 == pkg2


instance Ord Canonical where
  compare (Canonical pkg1 name1) (Canonical pkg2 name2) =
    case compare name1 name2 of
      LT -> LT
      EQ -> compare pkg1 pkg2
      GT -> GT


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
