{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, QuasiQuotes, ExtendedDefaultRules #-}


module Main where

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Text.InterpolatedString.Perl6 (qq)
import Parse.Parser
import Util.Arbs


main :: IO ()
main = defaultMain tests

parses :: String -> Bool
parses code = either (const False) (const True) (parseProgram code)

parseFails = not . parses

tests :: [TF.Test]
tests = [
    testGroup "String Assignments" [
        testProperty "prop_assignString" prop_assignString
      , testProperty "prop_invalidIdent" prop_invalidIdent
    ]
  ]

prop_assignString :: ValidIdent -> StringValue -> Bool
prop_assignString i s =
  parses [qq|{i} = "{s}"|]

prop_invalidIdent :: ValidIdent -> InvalidIdentChar -> StringValue -> Bool
prop_invalidIdent i z s =
  parseFails [qq|{i}{z} = "{s}"|]

