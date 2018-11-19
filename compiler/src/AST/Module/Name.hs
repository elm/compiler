{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Module.Name
  ( Canonical(..)
  , basics, char, string
  , maybe, result, list, array, dict, tuple
  , platform, cmd, sub
  , virtualDom, debug, bitwise
  , jsonDecode, jsonEncode
  , webgl, texture, vector2, vector3, vector4, matrix4
  , canonicalIsKernel
  )
  where


import Prelude hiding (maybe)
import Control.Monad (liftM2)
import Data.Binary
import qualified Data.Name as Name

import qualified Elm.Package as Pkg



-- NAMES


data Canonical =
  Canonical
    { _package :: !Pkg.Name
    , _module :: !Name.Name
    }
    deriving (Ord)


instance Eq Canonical where
  (==) (Canonical pkg home) (Canonical pkg' home') =
    home == home' && pkg == pkg'



-- PRIMITIVES


{-# NOINLINE basics #-}
basics :: Canonical
basics = Canonical Pkg.core "Basics"


{-# NOINLINE char #-}
char :: Canonical
char = Canonical Pkg.core Name.char


{-# NOINLINE string #-}
string :: Canonical
string = Canonical Pkg.core Name.string



-- CONTAINERS


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



-- EFFECTS


{-# NOINLINE platform #-}
platform :: Canonical
platform = Canonical Pkg.core Name.platform


{-# NOINLINE cmd #-}
cmd :: Canonical
cmd = Canonical Pkg.core "Platform.Cmd"


{-# NOINLINE sub #-}
sub :: Canonical
sub = Canonical Pkg.core "Platform.Sub"



-- MISC


{-# NOINLINE virtualDom #-}
virtualDom :: Canonical
virtualDom = Canonical Pkg.virtualDom Name.virtualDom


{-# NOINLINE debug #-}
debug :: Canonical
debug = Canonical Pkg.core Name.debug


{-# NOINLINE bitwise #-}
bitwise :: Canonical
bitwise = Canonical Pkg.core Name.bitwise



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



-- IS KERNEL


canonicalIsKernel :: Canonical -> Bool
canonicalIsKernel (Canonical _ name) =
  Name.isKernel name



-- BINARY


instance Binary Canonical where
  put (Canonical a b) =
    put a >> put b

  get =
    liftM2 Canonical get get
