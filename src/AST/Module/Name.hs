{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Module.Name
  ( Raw
  , Canonical(..)
  , inVirtualDom, inCore, inHtml, inBrowser
  , toString, toText, canonicalToText
  , isKernel, getKernel, canonicalIsKernel
  )
  where

import Data.Binary
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Elm.Package as Package



-- NAMES


type Raw = Text -- must be non-empty


data Canonical =
  Canonical
    { _package :: !Package.Name
    , _module :: !Raw
    }
    deriving (Eq, Ord)



-- HELPERS


inVirtualDom :: Raw -> Canonical
inVirtualDom raw =
  Canonical Package.virtualDom raw


inCore :: Raw -> Canonical
inCore raw =
  Canonical Package.core raw


inHtml :: Raw -> Canonical
inHtml raw =
  Canonical Package.html raw


inBrowser :: Raw -> Canonical
inBrowser raw =
  Canonical Package.browser raw



-- CONVERSIONS


toString :: Raw -> String
toString =
  Text.unpack


{-# INLINE toText #-}
toText :: Raw -> Text
toText name =
  name


canonicalToText :: Canonical -> Text
canonicalToText (Canonical _ name) =
  toText name



-- IS KERNEL


isKernel :: Raw -> Bool
isKernel name =
  Text.isPrefixOf "Elm.Kernel." name


getKernel :: Raw -> Text
getKernel name =
  Text.drop 11 name


canonicalIsKernel :: Canonical -> Bool
canonicalIsKernel (Canonical _ name) =
  isKernel name



-- BINARY


instance Binary Canonical where
  put (Canonical home name) =
    put home >> put name

  get =
    Canonical <$> get <*> get
