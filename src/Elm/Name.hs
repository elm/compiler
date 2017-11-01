{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Name
  ( Name
  , toString
  , int, float, bool, char, string
  , maybe, list, array
  )
  where


import Prelude hiding (maybe)
import qualified Data.Text as Text


type Name = Text.Text


toString :: Name -> String
toString =
  Text.unpack



-- COMMON NAMES


{-# NOINLINE int #-}
int :: Name
int = "Int"


{-# NOINLINE float #-}
float :: Name
float = "Float"


{-# NOINLINE bool #-}
bool :: Name
bool = "Bool"


{-# NOINLINE char #-}
char :: Name
char = "Char"


{-# NOINLINE string #-}
string :: Name
string = "String"


{-# NOINLINE maybe #-}
maybe :: Name
maybe = "Maybe"


{-# NOINLINE list #-}
list :: Name
list = "List"


{-# NOINLINE array #-}
array :: Name
array = "Array"
