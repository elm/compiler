module Parse.TypeTest where

import Test.Framework
import Test.Framework.Providers.HUnit
import Parse.TestHelpers

import Parse.Type
import AST.Type
import AST.Variable as Var


pending = at 0 0 0 0 $ RVar ""


example name input expected =
    testCase name $
        assertParse expr input expected


tests :: Test
tests =
    testGroup "Parse.Type"
    [ testGroup "unit"
        [ example "" "()" $ at 1 1 1 3 (RType (Raw "_Tuple0"))
        , example "whitespace" "( )" $ at 1 1 1 4 (RType (Raw "_Tuple0"))
        , example "comments" "({-A-})" $ at 1 1 1 8 (RType (Raw "_Tuple0"))
        , example "newlines" "(\n )" $ at 1 1 2 3 (RType (Raw "_Tuple0"))
        ]

    , testGroup "type variable"
        [ example "lowercase" "a" $ at 1 1 1 2 (RVar "a")
        ]

    , testGroup "constructor"
        [ example "" "Foo a b" $ at 1 1 1 8 (RApp (at 1 1 1 4 (RType (Var.Raw "Foo"))) [at 1 5 1 6 (RVar "a"),at 1 7 1 8 (RVar "b")])
        , example "no arguments" "Foo" $ at 1 1 1 4 (RType (Var.Raw "Foo"))
        , example "comments" "Foo{-A-}a{-B-}b" $ at 1 1 1 16 (RApp (at 1 1 1 4 (RType (Var.Raw "Foo"))) [at 1 9 1 10 (RVar "a"),at 1 15 1 16 (RVar "b")])
        , example "newlines" "Foo\n a\n b" $ at 1 1 3 3 (RApp (at 1 1 1 4 (RType (Var.Raw "Foo"))) [at 2 2 2 3 (RVar "a"),at 3 2 3 3 (RVar "b")])
        ]

    , testGroup "tuple constructor"
        [ example "single comma" "(,) a b" $ at 1 1 1 8 (RApp (at 1 1 1 4 (RType (Var.Raw "_Tuple2"))) [at 1 5 1 6 (RVar "a"),at 1 7 1 8 (RVar "b")])
        , example "multiple commas" "(,,,) a b c d" $ at 1 1 1 14 (RApp (at 1 1 1 6 (RType (Var.Raw "_Tuple4"))) [at 1 7 1 8 (RVar "a"),at 1 9 1 10 (RVar "b"),at 1 11 1 12 (RVar "c"),at 1 13 1 14 (RVar "d")])
        ]

    , testGroup "tuple type"
        [ example "" "(a,b)" $ at 1 1 1 6 (RApp (at 1 1 1 6 (RType (Var.Raw "_Tuple2"))) [at 1 2 1 3 (RVar "a"),at 1 4 1 5 (RVar "b")])
        , example "whitespace" "( a , b )" $ at 1 1 1 10 (RApp (at 1 1 1 10 (RType (Var.Raw "_Tuple2"))) [at 1 3 1 4 (RVar "a"),at 1 7 1 8 (RVar "b")])
        , example "comments" "({-A-}a{-B-},{-C-}b{-D-})" $ at 1 1 1 26 (RApp (at 1 1 1 26 (RType (Var.Raw "_Tuple2"))) [at 1 7 1 8 (RVar "a"),at 1 19 1 20 (RVar "b")])
        , example "newlines" "(\n a\n ,\n b\n )" $ at 1 1 5 3 (RApp (at 1 1 5 3 (RType (Var.Raw "_Tuple2"))) [at 2 2 2 3 (RVar "a"),at 4 2 4 3 (RVar "b")])
        ]

    , testGroup "record type"
        [ testGroup "empty"
            [ example "" "{}" $ at 1 1 1 3 (RRecord [] Nothing)
            , example "whitespace" "{ }" $ at 1 1 1 4 (RRecord [] Nothing)
            , example "comments" "{{-A-}}" $ at 1 1 1 8 (RRecord [] Nothing)
            ]

        , example "" "{x:m,y:n}" $ at 1 1 1 10 (RRecord [("x",at 1 4 1 5 (RVar "m")),("y",at 1 8 1 9 (RVar "n"))] Nothing)
        , example "single field" "{x:m}" $ at 1 1 1 6 (RRecord [("x",at 1 4 1 5 (RVar "m"))] Nothing)
        , example "whitespace" "{ x : m , y : n }" $ at 1 1 1 18 (RRecord [("x",at 1 7 1 8 (RVar "m")),("y",at 1 15 1 16 (RVar "n"))] Nothing)
        , example "comments" "{{-A-}x{-B-}:{-C-}m{-D-},{-E-}y{-F-}:{-G-}n{-H-}}" $ at 1 1 1 50 (RRecord [("x",at 1 19 1 20 (RVar "m")),("y",at 1 43 1 44 (RVar "n"))] Nothing)
        , example "single field with comments" "{{-A-}x{-B-}:{-C-}m{-D-}}" $ at 1 1 1 26 (RRecord [("x",at 1 19 1 20 (RVar "m"))] Nothing)
        , example "newlines" "{\n x\n :\n m\n ,\n y\n :\n n\n }" $ at 1 1 9 3 (RRecord [("x",at 4 2 4 3 (RVar "m")),("y",at 8 2 8 3 (RVar "n"))] Nothing)
        ]

    , testGroup "record extension"
        [ example "" "{a|x:m,y:n}" $ at 1 1 1 12 (RRecord [("x",at 1 6 1 7 (RVar "m")),("y",at 1 10 1 11 (RVar "n"))] (Just (at 1 2 1 3 (RVar "a"))))
        , example "single field" "{a|x:m}" $ at 1 1 1 8 (RRecord [("x",at 1 6 1 7 (RVar "m"))] (Just (at 1 2 1 3 (RVar "a"))))
        , example "whitespace" "{ a | x : m , y : n }" $ at 1 1 1 22 (RRecord [("x",at 1 11 1 12 (RVar "m")),("y",at 1 19 1 20 (RVar "n"))] (Just (at 1 3 1 4 (RVar "a"))))
        , example "comments" "{{-A-}a{-B-}|{-C-}x{-D-}:{-E-}m{-F-},{-G-}y{-H-}:{-I-}n{-J-}}" $ at 1 1 1 62 (RRecord [("x",at 1 31 1 32 (RVar "m")),("y",at 1 55 1 56 (RVar "n"))] (Just (at 1 7 1 8 (RVar "a"))))
        , example "newlines" "{\n a\n |\n x\n :\n m\n ,\n y\n :\n n\n }" $ at 1 1 11 3 (RRecord [("x",at 6 2 6 3 (RVar "m")),("y",at 10 2 10 3 (RVar "n"))] (Just (at 2 2 2 3 (RVar "a"))))
        , testCase "only allows simple base" $
            assertFailure expr "{()|x:m}"
        , testCase "only allows simple base" $
            assertFailure expr "{{}|x:m}"
        , testCase "must have fields" $
            assertFailure expr "{a|}"
        ]
    ]
