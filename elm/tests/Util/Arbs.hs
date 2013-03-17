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


newtype StringLiteral = StringLiteral String deriving (Eq, Show)

instance ShowQ StringLiteral where
  showQ (StringLiteral v) = v

instance Arbitrary StringLiteral where
  arbitrary = StringLiteral <$> (\x -> "\"" ++ x ++ "\"") <$> escape <$> arbitrary


newtype InvalidIdentChar = InvalidIdentChar Char deriving (Eq, Show)

instance ShowQ InvalidIdentChar where
  showQ (InvalidIdentChar c) = [c]

instance Arbitrary InvalidIdentChar where
  arbitrary = InvalidIdentChar <$> elements "!@#$%^&*()-=+\\|/?.>,<`~"
