module Test.Basics (tests) where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

tests : Test
tests =
    let comparison =
            suite "Comparison"
            [ test "max" <| assertEqual 42 (max 32 42)
            , test "min" <| assertEqual 42 (min 91 42)
            , test "clamp low" <| assertEqual 10 (clamp 10 20 5)
            , test "clamp mid" <| assertEqual 15 (clamp 10 20 15)
            , test "clamp high" <| assertEqual 20 (clamp 10 20 25)
            ]

    in
        suite "Basics"
            [ comparison
            ]
