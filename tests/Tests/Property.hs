module Tests.Property where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import SourceSyntax.Literal     (Literal)
import SourceSyntax.PrettyPrint (Pretty, pretty)
import Parse.Helpers            (IParser, iParse)
import Parse.Literal            (literal)
import Parse.Pattern            (expr)
import Tests.Property.Arbitrary

propertyTests :: Test
propertyTests =
  testGroup "Parse/Print Agreement Tests"
  [
    testProperty "Literal test" $ prop_parse_print literal
  , testProperty "Pattern test" $ prop_parse_print expr
  ]

prop_parse_print :: (Pretty a, Arbitrary a, Eq a) => IParser a -> a -> Bool
prop_parse_print p x =
  either (const False) (== x) . parse_print p $ x

parse_print :: (Pretty a) => IParser a -> a -> Either String a
parse_print p = either (Left . show) (Right) . iParse p . show . pretty
