{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Module.Name
  ( Canonical(..)
  , basics, maybe, list, array
  , platform, cmd, sub
  , jsonDecode, jsonEncode
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



-- HELPERS


{-# NOINLINE basics #-}
basics :: Canonical
basics = Canonical Pkg.core "Basics"


{-# NOINLINE maybe #-}
maybe :: Canonical
maybe = Canonical Pkg.core "Maybe"


{-# NOINLINE list #-}
list :: Canonical
list = Canonical Pkg.core "List"


{-# NOINLINE array #-}
array :: Canonical
array = Canonical Pkg.core "Array"


{-# NOINLINE platform #-}
platform :: Canonical
platform = Canonical Pkg.core "Platform"


{-# NOINLINE cmd #-}
cmd :: Canonical
cmd = Canonical Pkg.core "Platform.Cmd"


{-# NOINLINE sub #-}
sub :: Canonical
sub = Canonical Pkg.core "Platform.Sub"


{-# NOINLINE jsonDecode #-}
jsonDecode :: Canonical
jsonDecode = Canonical Pkg.core "Json.Decode"


{-# NOINLINE jsonEncode #-}
jsonEncode :: Canonical
jsonEncode = Canonical Pkg.core "Json.Encode"



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
