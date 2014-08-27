module Test.CodeGen (tests) where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Maybe
import Maybe (..)

casePrime m' =
    case m' of
        Just x -> x
        Nothing -> 0

patternPrime =
    case Just 42 of
        Just x' -> x'
        Nothing -> 0

letQualified =
    let (Maybe.Just x) = Just 42
    in  x

caseQualified =
    case Just 42 of
        Maybe.Just x -> x
        Nothing -> 0

tests : Test
tests =
    let primes =
            suite "Primes"
            [ test "case" <| assertEqual 42 (casePrime (Just 42))
            , test "pattern" <| assertEqual 42 patternPrime
            ]
        qualifiedPatterns =
            suite "Qualified Patterns"
            [ test "let" <| assertEqual 42 letQualified
            , test "case" <| assertEqual 42 caseQualified
            ]
    in
        suite "CodeGen"
            [ primes
            , qualifiedPatterns
            ]
