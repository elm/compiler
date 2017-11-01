{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Module.Name
  ( Canonical(..)
  , basics, maybe, list, array
  , jsonDecode, jsonEncode
  , cmd, sub
  , canonicalToText
  , isKernel, getKernel, canonicalIsKernel
  )
  where


import Prelude hiding (maybe)
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


basics :: Canonical
basics =
  Canonical Pkg.core "Basics"


maybe :: Canonical
maybe =
  Canonical Pkg.core "Maybe"


list :: Canonical
list =
  Canonical Pkg.core "List"


array :: Canonical
array =
  Canonical Pkg.core "Array"


jsonDecode :: Canonical
jsonDecode =
  Canonical Pkg.core "Json.Decode"


jsonEncode :: Canonical
jsonEncode =
  Canonical Pkg.core "Json.Encode"


cmd :: Canonical
cmd =
  Canonical Pkg.core "Platform.Cmd"


sub :: Canonical
sub =
  Canonical Pkg.core "Platform.Sub"



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
    Canonical <$> get <*> get
