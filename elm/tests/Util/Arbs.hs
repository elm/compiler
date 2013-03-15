module Util.Arbs where

import Control.Applicative
import Test.QuickCheck
import Util.CharGens
import Util.Stringable
import Util.String


newtype ValidIdent = ValidIdent String deriving (Eq, Show)

instance Stringable ValidIdent where
  stringify (ValidIdent v) = v

instance Arbitrary ValidIdent where
  arbitrary = ValidIdent <$> ((:) <$> lowerAlpha <*> (listOf alphaNumUnderPrime))


newtype StringValue = StringValue String deriving (Eq, Show)

instance Stringable StringValue where
  stringify (StringValue v) = v

instance Arbitrary StringValue where
  arbitrary = StringValue <$> escape <$> arbitrary


newtype InvalidIdentChar = InvalidIdentChar Char deriving (Eq, Show)

instance Stringable InvalidIdentChar where
  stringify (InvalidIdentChar c) = [c]

instance Arbitrary InvalidIdentChar where
  arbitrary = InvalidIdentChar <$> elements "!@#$%^&*()-=+\\|/?.>,<`~"
