{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, QuasiQuotes, ExtendedDefaultRules #-}


module Main where

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Text.InterpolatedString.Perl6 (qq)
import Parse.Parser
import Util.Arbs

import Debug.Trace

main :: IO ()
main = defaultMain tests

parses :: String -> Bool
parses code = either (const False) (const True) (parseProgram code)

parseFails :: String -> Bool
parseFails = not . parses

tests :: [TF.Test]
tests = [
    testGroup "String Assignments" [
        testProperty "prop_assignString" prop_assignString
      , testProperty "prop_invalidIdent" prop_invalidIdent
      , testProperty "prop_emptyList" prop_emptyList
      , testProperty "prop_record" prop_record
    ]
  ]

prop_assignString :: ValidIdent -> StringLiteral -> Bool
prop_assignString i s = parses [qq|
{i} = {s}
|]

prop_invalidIdent :: ValidIdent -> InvalidIdentChar -> StringLiteral -> Bool
prop_invalidIdent i z s = parseFails [qq|
{i}{z} = {s}
|]

prop_emptyList :: ValidIdent -> Bool
prop_emptyList i = parses [qq|
{i}=[]
|]

prop_record :: ValidIdent -> ValidIdent -> ValidIdent -> ValidIdent -> StringLiteral -> Bool
prop_record a b c k v = parses [qq|
{a} = \{{k} = {v} \}
{b} = .{k} {a}
{c} = {a}.{k}
|]
