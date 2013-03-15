{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Util.Stringable where

class Stringable s where
  stringify :: s -> String

instance Stringable String where
  stringify = id

infixr 5 +-
(+-) :: (Stringable a, Stringable b) => a -> b -> String
a +- b = (stringify a) ++ (stringify b)

