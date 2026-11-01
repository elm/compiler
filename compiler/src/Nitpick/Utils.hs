{-# LANGUAGE PatternSynonyms #-}
module Nitpick.Utils
  ( Variants(..)
  , Variant(..)
  , pattern Variants_
  , NumAlts(..)
  , vector
  , box
  , void
  )
  where


import qualified AST.Prim.Name as N
import qualified AST.Prim.Tag as Tag



-- VARIANTS
--
-- During exhaustiveness checking, some additional information is needed about
-- the full set of possible variants. This is the minimal set of information
-- necessary as of (23 Nov 2020)
--


data Variants =
  Variants NumAlts [Variant]


data Variant =
  Variant N.Name Tag.Tag Int


newtype NumAlts = NumAlts Int
  deriving (Eq, Ord)


{-# COMPLETE Variants_ #-}
pattern Variants_ :: Int -> [Variant] -> Variants
pattern Variants_ n vs = Variants (NumAlts n) vs



-- HELPERS


vector :: Int -> N.Name -> Variants
vector len name =
  Variants (NumAlts 1) [Variant name Tag.zero len]


box :: N.Name -> Variants
box =
  vector 1


void :: N.Name -> Variants
void =
  vector 0
