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
    in suite "Equality Tests" []