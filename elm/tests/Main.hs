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

import Util.Arbs
import Util.CharGens
import Util.Stringable


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

-- TODO: variable whitespace?
prop_assignString :: ValidIdent -> StringValue -> Bool
prop_assignString vi sv =
  parses $ vi +- " = \"" +- sv +- "\""


prop_invalidIdent :: ValidIdent -> InvalidIdentChar -> StringValue -> Bool
prop_invalidIdent vi i s =
  (not . parses) $ vi +- i +- " = \"" +- s +- "\""

