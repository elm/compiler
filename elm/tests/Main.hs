{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Control.Applicative

import Data.Maybe (listToMaybe, isJust)
import Data.Functor.Identity
import Data.Text

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Property

import Text.Regex

import Parse.Parser

import Debug.Trace

import Util.Stringable
import Util.CharGens


main :: IO ()
main = defaultMain tests

parses :: String -> Bool
parses code = either (const False) (const True) (parseProgram code)

tests :: [TF.Test]
tests = [
    testGroup "String Assignments" [
      testProperty "prop_assignString" prop_assignString,
      testProperty "prop_invalidIdent" prop_invalidIdent
    ]
  ]


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


replace' :: String -> String -> String -> String
replace' a b c = unpack $ replace (pack a) (pack b) (pack c)

escape :: String -> String
escape =
  (replace' "\n" "\\n") .
  (replace' "\t" "\\t") .
  (replace' "\"" "\\\"") .
  (replace' "\\" "\\\\")


-- TODO: variable whitespace?
prop_assignString :: ValidIdent -> StringValue -> Bool
prop_assignString vi sv =
  parses $ vi +- " = \"" +- sv +- "\""


prop_invalidIdent :: ValidIdent -> InvalidIdentChar -> StringValue -> Bool
prop_invalidIdent vi i s =
  (not . parses) $ vi +- i +- " = \"" +- s +- "\""

