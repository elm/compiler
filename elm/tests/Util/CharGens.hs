module Util.CharGens where

import Test.QuickCheck
import Test.QuickCheck.Property

lowerAlpha :: Gen Char
lowerAlpha = elements ['a'..'z']

upperAlpha :: Gen Char
upperAlpha = elements ['A'..'Z']

alpha :: Gen Char
alpha = oneof [lowerAlpha, upperAlpha]

alphaNum :: Gen Char
alphaNum = oneof [alpha, elements ['0'..'9']]

alphaNumUnderPrime :: Gen Char
alphaNumUnderPrime = oneof [alphaNum, elements ['_', '\'']]
