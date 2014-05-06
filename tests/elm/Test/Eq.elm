module Test.Eq (tests) where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

data Different = A String 
               | B [Int]

tests : Test
tests = 
    let diff_tests = suite "sum type eq" 
                     [ test "As eq" <| assert (A "a" == A "a")
                     , test "Bs eq" <| assert (B [1] == B [1])
                     , test "A left neq" <| assert . not <| (A "a" == B [1])
                     , test "A left neq" <| assert . not <| (B [1] == A "a")
                     ]
        record_tests = suite "record type eq"
                       [ test "empty same" <| assert ({} == {})
                       , test "ctor same"  <| assert ({ctor = Just 3} == {ctor = Just 3})
                       , test "ctor diff"  . assert . not <| ({ctor = Just 3} == {ctor = Nothing})
                       ]
    in suite "Equality Tests" [diff_tests, record_tests]