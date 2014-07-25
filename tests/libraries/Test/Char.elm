module Test.Char (tests) where

import Char

import ElmTest.Assertion (..)
import ElmTest.Test (..)

tests : Test
tests = 
    suite "Char" 
          [ test "toUpper" <| assertEqual 'C' <| Char.toUpper 'c'
          , test "toLower" <| assertEqual 'c' <| Char.toLower 'C'
          , test "toLocaleUpper" <| assertEqual 'C' <| Char.toLocaleUpper 'c'
          , test "toLocaleLower" <| assertEqual 'c' <| Char.toLocaleLower 'C'
          ]
