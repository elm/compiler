{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Module.Name
  ( Canonical(..)
  , basics, char, string
  , maybe, list, array
  , platform, cmd, sub
  , jsonDecode, jsonEncode
  , webgl, vector2, vector3, vector4, matrix4
  , canonicalToText
  , isKernel, getKernel, canonicalIsKernel
  )
  where


import Prelude hiding (maybe)
import Control.Monad (liftM2)
import Data.Binary
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Elm.Name as N
import qualified Elm.Package as Pkg



-- NAMES


data Canonical =
  Canonical
    { _package :: !Pkg.Name
    , _module :: !N.Name
    }
    deriving (Eq, Ord)



-- PRIMITIVES


{-# NOINLINE basics #-}
basics :: Canonical
basics = Canonical Pkg.core "Basics"


{-# NOINLINE char #-}
char :: Canonical
char = Canonical Pkg.core "Char"


{-# NOINLINE string #-}
string :: Canonical
string = Canonical Pkg.core "String"



-- CONTAINERS


{-# NOINLINE maybe #-}
maybe :: Canonical
maybe = Canonical Pkg.core "Maybe"


{-# NOINLINE list #-}
list :: Canonical
list = Canonical Pkg.core "List"


{-# NOINLINE array #-}
array :: Canonical
array = Canonical Pkg.core "Array"



-- EFFECTS


{-# NOINLINE platform #-}
platform :: Canonical
platform = Canonical Pkg.core "Platform"


{-# NOINLINE cmd #-}
cmd :: Canonical
cmd = Canonical Pkg.core "Platform.Cmd"


{-# NOINLINE sub #-}
sub :: Canonical
sub = Canonical Pkg.core "Platform.Sub"



-- JSON


{-# NOINLINE jsonDecode #-}
jsonDecode :: Canonical
jsonDecode = Canonical Pkg.core "Json.Decode"


{-# NOINLINE jsonEncode #-}
jsonEncode :: Canonical
jsonEncode = Canonical Pkg.core "Json.Encode"



-- WEBGL


{-# NOINLINE webgl #-}
webgl :: Canonical
webgl = Canonical Pkg.webgl "WebGL"


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



-- CONVERSIONS


canonicalToText :: Canonical -> Text
canonicalToText (Canonical _ name) =
  name



-- IS KERNEL


isKernel :: N.Name -> Bool
isKernel name =
  Text.isPrefixOf "Elm.Kernel." name


getKernel :: N.Name -> Text
getKernel name =
  Text.drop 11 name


canonicalIsKernel :: Canonical -> Bool
canonicalIsKernel (Canonical _ name) =
  isKernel name



-- BINARY


instance Binary Canonical where
  put (Canonical a b) =
    put a >> put b

  get =
    liftM2 Canonical get get
