{-# LANGUAGE BangPatterns #-}
module Elm.ModuleName
  ( Canonical(..)
  , basics, char, string
  , maybe, result, list, array, dict, tuple
  , platform, platform_cmd, platform_sub
  , debug
  , virtualDom
  , jsonDecode, jsonEncode
  , webgl, texture, vector2, vector3, vector4, matrix4
  --
  , eCanonical, dCanonical
  )
  where


import Prelude hiding (maybe)
import Control.Monad (liftM2)

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified ThreadSafe.Fork as Fork

import qualified AST.Prim.Module as Module
import qualified Elm.Package as Pkg



-- CANONICAL


data Canonical =
  Canonical
    { _package :: !Pkg.Name
    , _module :: !Module.Name
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


instance Fork.Context Canonical where
  toContextChars (Canonical pkg home) =
    Module.toChars home ++ " in " ++ Pkg.toChars pkg



-- BINARY FORMAT


eCanonical :: Canonical -> E.Builder
eCanonical (Canonical p h) =
  Pkg.eName p <> Module.eName h


dCanonical :: D.Decoder Canonical
dCanonical =
  liftM2 Canonical Pkg.dName Module.dName



-- CORE


{-# NOINLINE basics       #-}; basics       :: Canonical; basics       = Canonical Pkg.core Module.basics
{-# NOINLINE char         #-}; char         :: Canonical; char         = Canonical Pkg.core Module.char
{-# NOINLINE string       #-}; string       :: Canonical; string       = Canonical Pkg.core Module.string
{-# NOINLINE maybe        #-}; maybe        :: Canonical; maybe        = Canonical Pkg.core Module.maybe
{-# NOINLINE result       #-}; result       :: Canonical; result       = Canonical Pkg.core Module.result
{-# NOINLINE list         #-}; list         :: Canonical; list         = Canonical Pkg.core Module.list
{-# NOINLINE array        #-}; array        :: Canonical; array        = Canonical Pkg.core Module.array
{-# NOINLINE dict         #-}; dict         :: Canonical; dict         = Canonical Pkg.core Module.dict
{-# NOINLINE tuple        #-}; tuple        :: Canonical; tuple        = Canonical Pkg.core Module.tuple
{-# NOINLINE platform     #-}; platform     :: Canonical; platform     = Canonical Pkg.core Module.platform
{-# NOINLINE platform_cmd #-}; platform_cmd :: Canonical; platform_cmd = Canonical Pkg.core Module.platform_cmd
{-# NOINLINE platform_sub #-}; platform_sub :: Canonical; platform_sub = Canonical Pkg.core Module.platform_sub
{-# NOINLINE debug        #-}; debug        :: Canonical; debug        = Canonical Pkg.core Module.debug



-- HTML


{-# NOINLINE virtualDom #-}; virtualDom :: Canonical; virtualDom = Canonical Pkg.virtualDom Module.virtualDom



-- JSON


{-# NOINLINE jsonDecode #-}; jsonDecode :: Canonical; jsonDecode = Canonical Pkg.json Module.json_decode
{-# NOINLINE jsonEncode #-}; jsonEncode :: Canonical; jsonEncode = Canonical Pkg.json Module.json_encode



-- WEBGL


{-# NOINLINE webgl   #-}; webgl   :: Canonical; webgl   = Canonical Pkg.webgl         Module.webgl
{-# NOINLINE texture #-}; texture :: Canonical; texture = Canonical Pkg.webgl         Module.webgl_texture
{-# NOINLINE vector2 #-}; vector2 :: Canonical; vector2 = Canonical Pkg.linearAlgebra Module.math_vector2
{-# NOINLINE vector3 #-}; vector3 :: Canonical; vector3 = Canonical Pkg.linearAlgebra Module.math_vector3
{-# NOINLINE vector4 #-}; vector4 :: Canonical; vector4 = Canonical Pkg.linearAlgebra Module.math_vector4
{-# NOINLINE matrix4 #-}; matrix4 :: Canonical; matrix4 = Canonical Pkg.linearAlgebra Module.math_matrix4


