module Util.Arbs where

import Control.Applicative
import Test.QuickCheck
import Util.CharGens
import Util.String

import Text.InterpolatedString.Perl6 (ShowQ(showQ))


newtype ValidIdent = ValidIdent String deriving (Eq, Show)

instance ShowQ ValidIdent where
  showQ (ValidIdent v) = v

instance Arbitrary ValidIdent where
  arbitrary = ValidIdent <$> ((:) <$> lowerAlpha <*> (listOf alphaNumUnderPrime))


newtype StringValue = StringValue String deriving (Eq, Show)

instance ShowQ StringValue where
  showQ (StringValue v) = v

instance Arbitrary StringValue where
  arbitrary = StringValue <$> escape <$> arbitrary


newtype InvalidIdentChar = InvalidIdentChar Char deriving (Eq, Show)

instance ShowQ InvalidIdentChar where
  showQ (InvalidIdentChar c) = [c]

instance Arbitrary InvalidIdentChar where
  arbitrary = InvalidIdentChar <$> elements "!@#$%^&*()-=+\\|/?.>,<`~"
