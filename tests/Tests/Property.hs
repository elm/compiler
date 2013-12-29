module Tests.Property where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import SourceSyntax.Literal     (Literal)
import SourceSyntax.PrettyPrint (pretty)
import Parse.Helpers            (iParse)
import Parse.Literal            (literal)
import Tests.Property.Arbitrary

propertyTests :: Test
propertyTests =
  testGroup "Parse/Print Agreement Tests"
  [
    testProperty "Literal test" prop_literal_parse_print
  ]

prop_literal_parse_print :: Literal -> Bool
prop_literal_parse_print l =
  either (const False) (== l) . iParse literal . show . pretty $ l
