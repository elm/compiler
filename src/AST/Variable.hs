{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Variable
  ( Global(..)
  )
  where


import Data.Binary

import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N



-- GLOBAL NAMES


data Global = Global !ModuleName.Canonical !N.Name
  deriving (Eq, Ord)



-- BINARY SERIALIZATION


instance Binary Global where
  put (Global home name) =
    put home >> put name

  get =
    Global <$> get <*> get
