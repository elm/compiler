{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Property.Arbitrary where

import Control.Applicative       ((<$>))
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import SourceSyntax.Literal

instance Arbitrary Literal where
  arbitrary = oneof [ IntNum   <$> arbitrary
                    , FloatNum <$> arbitrary
                    , Chr      <$> arbitrary
                    , Str      <$> arbitrary
                      -- Booleans aren't actually source syntax 
                      -- , Boolean  <$> arbitrary
                    ]
  shrink l = case l of
    IntNum n   -> IntNum   <$> shrink n
    FloatNum f -> FloatNum <$> shrink f
    Chr c      -> Chr      <$> shrink c
    Str s      -> Str      <$> shrink s
    Boolean b  -> Boolean  <$> shrink b
