module Parse.TestHelpers where

import Test.HUnit (Assertion, assertEqual)

import Parse.Helpers (IParser, iParse)
import Reporting.Annotation hiding (map, at)
import Reporting.Region
import Text.ParserCombinators.Parsec.Combinator (eof)


parseFullInput :: IParser a -> IParser a
parseFullInput parser =
    (\x _ -> x) <$> parser <*> eof


assertParse :: (Show a, Eq a) => IParser a -> String -> a -> Assertion
assertParse parser input expected =
    let
        output = iParse (parseFullInput parser) input
    in
        case output of
            Left err ->
                assertEqual (show err) False True
            Right result ->
                assertEqual input expected result


assertFailure :: (Show a, Eq a) => IParser a -> String -> Assertion
assertFailure parser input =
    let
        output = iParse (parseFullInput parser) input
    in
        case output of
            Left err ->
                assertEqual (show err) True True
            Right result ->
                assertEqual (show result) True False


nowhere = Region (Position 0 0) (Position 0 0)

at a b c d = A (Region (Position a b) (Position c d))
