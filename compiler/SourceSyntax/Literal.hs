{-# OPTIONS_GHC -Wall #-}
module SourceSyntax.Literal where

import SourceSyntax.PrettyPrint
import qualified Text.PrettyPrint as PP

data Literal = IntNum Int
             | FloatNum Double
             | Chr Char
             | Str String
             | Boolean Bool
             deriving (Eq, Ord, Show)

instance Pretty Literal where
  pretty literal =
    case literal of
      IntNum n -> PP.int n
      FloatNum n -> PP.double n
      Chr c -> PP.text . show $ c
      Str s -> PP.text . show $ s
      Boolean bool -> PP.text (show bool)
