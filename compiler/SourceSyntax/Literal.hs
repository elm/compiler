{-# LANGUAGE DeriveDataTypeable #-}
module SourceSyntax.Literal where

import Data.Data

data Literal = IntNum Int
             | FloatNum Float
             | Chr Char
             | Str String
             | Boolean Bool
               deriving (Eq, Data, Typeable)


instance Show Literal where
  show e =
      case e of
        IntNum n -> show n
        FloatNum n -> show n
        Chr c -> show c
        Str s -> show s
        Boolean b -> show b