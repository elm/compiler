module Parse.PatternTest where

import Test.Framework
import Test.Framework.Providers.HUnit
import Parse.TestHelpers

import Parse.Pattern
import AST.Pattern
import AST.Variable hiding (Alias)
import AST.Literal


pending = at 0 0 0 0 Anything

example name input expected =
    testCase name $
        assertParse expr input expected


tests :: Test
tests =
    testGroup "Parse.Pattern"
    [ example "wildcard" "_" $ at 1 1 1 2 Anything

    , example "literal" "1" $ at 1 1 1 2 (Literal (IntNum 1))

    , example "variable" "a" $ at 1 1 1 2 (Var "a")

    , testGroup "data"
        [ example "" "Just _" $ at 1 1 1 7 (Data (Raw "Just") [at 1 6 1 7 Anything])
        , example "comments" "Just{-A-}_" $ at 1 1 1 11 (Data (Raw "Just") [at 1 10 1 11 Anything])
        , example "newlines" "Just\n _" $ at 1 1 2 3 (Data (Raw "Just") [at 2 2 2 3 Anything])

        , testGroup "infix constructor"
            [ example "" "x::y" $ at 1 1 1 5 (Data (Raw "::") [at 1 1 1 2 (Var "x"),at 1 4 1 5 (Var "y")])
            , example "whitespace" "x :: y" $ at 1 1 1 7 (Data (Raw "::") [at 1 1 1 2 (Var "x"),at 1 6 1 7 (Var "y")])
            , example "comments" "x{-A-}::{-B-}y" $ at 1 1 1 15 (Data (Raw "::") [at 1 1 1 2 (Var "x"),at 1 14 1 15 (Var "y")])
            , example "newlines" "x\n ::\n y" $ at 1 1 3 3 (Data (Raw "::") [at 1 1 1 2 (Var "x"),at 3 2 3 3 (Var "y")])
            ]
        ]

    , testGroup "unit"
        [ example "" "()" $ at 1 1 1 3 (Data (Raw "_Tuple0") [])
        , example "whitespace" "( )" $ at 1 1 1 4 (Data (Raw "_Tuple0") [])
        , example "comments" "({-A-})" $ at 1 1 1 8 (Data (Raw "_Tuple0") [])
        , example "newlines" "(\n )" $ at 1 1 2 3 (Data (Raw "_Tuple0") [])
        ]

    , testGroup "parentheses"
        [ example "" "(_)" $ at 1 2 1 3 Anything
        , example "whitespace" "( _ )" $ at 1 3 1 4 Anything
        , example "comments" "({-A-}_{-B-})" $ at 1 7 1 8 Anything
        , example "newlines" "(\n _\n )" $ at 2 2 2 3 Anything
        ]

    , testGroup "tuple"
        [ example "" "(x,y)" $ at 1 1 1 6 (Data (Raw "_Tuple2") [at 1 2 1 3 (Var "x"),at 1 4 1 5 (Var "y")])
        , example "whitespace" "( x , y )" $ at 1 1 1 10 (Data (Raw "_Tuple2") [at 1 3 1 4 (Var "x"),at 1 7 1 8 (Var "y")])
        , example "comments" "({-A-}x{-B-},{-C-}y{-D-})" $ at 1 1 1 26 (Data (Raw "_Tuple2") [at 1 7 1 8 (Var "x"),at 1 19 1 20 (Var "y")])
        , example "newlines" "(\n x\n ,\n y\n )" $ at 1 1 5 3 (Data (Raw "_Tuple2") [at 2 2 2 3 (Var "x"),at 4 2 4 3 (Var "y")])
        ]

    , testGroup "list"
        [ example "" "[x,y]" $ at 1 2 1 5 (Data (Raw "::") [at 1 2 1 3 (Var "x"),at 1 4 1 5 (Data (Raw "::") [at 1 4 1 5 (Var "y"),at 1 5 1 5 (Data (Raw "[]") [])])])
        , example "no elements" "[]" $ at 1 2 1 2 (Data (Raw "[]") [])
        , example "single element" "[x]" $ at 1 2 1 3 (Data (Raw "::") [at 1 2 1 3 (Var "x"),at 1 3 1 3 (Data (Raw "[]") [])])
        , example "whitespace" "[ x , y ]" $ at 1 3 1 8 (Data (Raw "::") [at 1 3 1 4 (Var "x"),at 1 7 1 8 (Data (Raw "::") [at 1 7 1 8 (Var "y"),at 1 8 1 8 (Data (Raw "[]") [])])])
        , example "comments" "[{-A-}x{-B-},{-C-}y{-D-}]" $ at 1 7 1 20 (Data (Raw "::") [at 1 7 1 8 (Var "x"),at 1 19 1 20 (Data (Raw "::") [at 1 19 1 20 (Var "y"),at 1 20 1 20 (Data (Raw "[]") [])])])
        , example "newlines" "[\n x\n ,\n y\n ]" $ at 2 2 4 3 (Data (Raw "::") [at 2 2 2 3 (Var "x"),at 4 2 4 3 (Data (Raw "::") [at 4 2 4 3 (Var "y"),at 4 3 4 3 (Data (Raw "[]") [])])])
        ]

    , testGroup "record"
        [ example "" "{a,b}" $ at 1 1 1 6 (Record ["a","b"])
        , example "single element" "{a}" $ at 1 1 1 4 (Record ["a"])
        , example "whitespace" "{ a , b }" $ at 1 1 1 10 (Record ["a","b"])
        , example "comments" "{{-A-}a{-B-},{-C-}b{-D-}}" $ at 1 1 1 26 (Record ["a","b"])
        , example "newlines" "{\n a\n ,\n b\n }" $ at 1 1 5 3 (Record ["a","b"])
        , testCase "must have at least one field" $
            assertFailure expr "{}"
        ]

    , testGroup "alias"
        [ example "" "_ as x" $ at 1 1 1 7 (Alias "x" (at 1 1 1 2 Anything))
        , example "left side has whitespace" "A b as x" $ at 1 1 1 9 (Alias "x" (at 1 1 1 4 (Data (Raw "A") [at 1 3 1 4 (Var "b")])))
        , example "left side ctor without whitespace" "A as x" $ at 1 1 1 7 (Alias "x" (at 1 1 1 2 (Data (Raw "A") [])))
        , example "comments" "_{-A-}as{-B-}x" $ at 1 1 1 15 (Alias "x" (at 1 1 1 2 Anything))
        , example "newlines" "_\n as\n x" $ at 1 1 3 3 (Alias "x" (at 1 1 1 2 Anything))
        , example "nested" "(_ as x)as y" $ at 1 2 1 13 (Alias "y" (at 1 2 1 8 (Alias "x" (at 1 2 1 3 Anything))))
        , example "nested (whitespace)" "(_ as x) as y" $ at 1 2 1 14 (Alias "y" (at 1 2 1 8 (Alias "x" (at 1 2 1 3 Anything))))
        , testCase "nesting required parentheses" $
            assertFailure expr "_ as x as y"
        ]
    ]
