module Parse.ExpressionTest where

import Test.Framework
import Test.Framework.Providers.HUnit
import Parse.TestHelpers

import Parse.Expression
import AST.Expression.General
import AST.Expression.Source
import AST.Variable
import AST.Literal
import AST.Pattern hiding (Data, Literal, Record, Var)
import qualified AST.Pattern as P
import Text.Parsec.Char (string)


pending = at 0 0 0 0 $ Literal $ IntNum 0


example name input expected =
    testCase name $
        assertParse expr input expected


tests :: Test
tests =
    testGroup "Parse.Expression"
    [ testGroup "Unit"
        [ example "" "()" $ at 1 1 1 3 (Data "_Tuple0" [])
        , example "whitespace" "( )" $ at 1 1 1 4 (Data "_Tuple0" [])
        , example "comments" "({-A-})" $ at 1 1 1 8 (Data "_Tuple0" [])
        , example "newlines" "(\n )" $ at 1 1 2 3 (Data "_Tuple0" [])
        ]

    , testGroup "Literal"
        [ example "" "1" $ at 1 1 1 2 (Literal (IntNum 1))

        , testGroup "Boolean"
            [ example "True" "True" $ at 1 1 1 5 (Literal (Boolean True))
            , example "False" "False" $ at 1 1 1 6 (Literal (Boolean False))
            ]
        ]

    , testGroup "variable"
        [ example "lowercase" "foo" $ at 1 1 1 4 (Var (Raw "foo"))
        , example "uppercase" "Bar" $ at 1 1 1 4 (Var (Raw "Bar"))
        , example "qualified" "Bar.Baz.foo" $ at 1 5 1 12 (Var (Raw "Bar.Baz.foo"))

        , testGroup "symbolic operator"
            [ example "" "(+)" $ at 1 1 1 4 (Lambda (at 1 1 1 4 (P.Var "x")) (at 1 1 1 4 (Lambda (at 1 1 1 4 (P.Var "y")) (at 1 1 1 4 (Binop (Raw "+") (at 1 1 1 4 (Var (Raw "x"))) (at 1 1 1 4 (Var (Raw "y"))))))))
            , example "whitespace" "( + )" $ at 1 1 1 6 (Lambda (at 1 1 1 6 (P.Var "x")) (at 1 1 1 6 (Lambda (at 1 1 1 6 (P.Var "y")) (at 1 1 1 6 (Binop (Raw "+") (at 1 1 1 6 (Var (Raw "x"))) (at 1 1 1 6 (Var (Raw "y"))))))))
            , example "comments" "({-A-}+{-B-})" $ at 1 1 1 14 (Lambda (at 1 1 1 14 (P.Var "x")) (at 1 1 1 14 (Lambda (at 1 1 1 14 (P.Var "y")) (at 1 1 1 14 (Binop (Raw "+") (at 1 1 1 14 (Var (Raw "x"))) (at 1 1 1 14 (Var (Raw "y"))))))))
            , example "newlines" "(\n + \n )" $ at 1 1 3 3 (Lambda (at 1 1 3 3 (P.Var "x")) (at 1 1 3 3 (Lambda (at 1 1 3 3 (P.Var "y")) (at 1 1 3 3 (Binop (Raw "+") (at 1 1 3 3 (Var (Raw "x"))) (at 1 1 3 3 (Var (Raw "y"))))))))
            ]
        ]

    , testGroup "function application"
        [ example "" "f 7 8" $ at 1 1 1 6 (App (at 1 1 1 4 (App (at 1 1 1 2 (Var (Raw "f"))) (at 1 3 1 4 (Literal (IntNum 7))))) (at 1 5 1 6 (Literal (IntNum 8))))
        , example "argument starts with minus" "f -9 -x" $ at 1 1 1 8 (App (at 1 1 1 5 (App (at 1 1 1 2 (Var (Raw "f"))) (at 1 3 1 5 (Literal (IntNum (-9)))))) (at 1 6 1 8 (Binop (Raw "-") (at 1 6 1 8 (Literal (IntNum 0))) (at 1 7 1 8 (Var (Raw "x"))))))
        , example "comments" "f{-A-}7{-B-}8" $ at 1 1 1 14 (App (at 1 1 1 8 (App (at 1 1 1 2 (Var (Raw "f"))) (at 1 7 1 8 (Literal (IntNum 7))))) (at 1 13 1 14 (Literal (IntNum 8))))
        , example "newlines" "f\n 7\n 8" $ at 1 1 3 3 (App (at 1 1 2 3 (App (at 1 1 1 2 (Var (Raw "f"))) (at 2 2 2 3 (Literal (IntNum 7))))) (at 3 2 3 3 (Literal (IntNum 8))))
        , example "newlines and comments" "f\n {-A-}7\n {-B-}8" $ at 1 1 3 8 (App (at 1 1 2 8 (App (at 1 1 1 2 (Var (Raw "f"))) (at 2 7 2 8 (Literal (IntNum 7))))) (at 3 7 3 8 (Literal (IntNum 8))))
        ]

    , testGroup "unary operators"
        [ testGroup "negative"
            [ example "" "-True" $ at 1 1 1 6 (Binop (Raw "-") (at 1 1 1 6 (Literal (IntNum 0))) (at 1 2 1 6 (Literal (Boolean True))))
            , testCase "must not have whitespace" $
                assertFailure expr "- True"
            , testCase "must not have comment" $
                assertFailure expr "-{- -}True"
            , testCase "does not apply to '-'" $
                assertFailure expr "--True"
            , testCase "does not apply to '.'" $
                assertFailure expr "-.foo"
            ]
        ]

    , testGroup "binary operators"
        [ example "" "7+8<<>>9" $ at 1 1 1 9 (Binop (Raw "<<>>") (at 1 1 1 4 (Binop (Raw "+") (at 1 1 1 2 (Literal (IntNum 7))) (at 1 3 1 4 (Literal (IntNum 8))))) (at 1 8 1 9 (Literal (IntNum 9))))
        , example "minus with no whitespace" "9-1" $ at 1 1 1 4 (Binop (Raw "-") (at 1 1 1 2 (Literal (IntNum 9))) (at 1 3 1 4 (Literal (IntNum 1))))
        , example "backticks" "7`plus`8`shift`9" $ at 1 1 1 17 (Binop (Raw "shift") (at 1 1 1 9 (Binop (Raw "plus") (at 1 1 1 2 (Literal (IntNum 7))) (at 1 8 1 9 (Literal (IntNum 8))))) (at 1 16 1 17 (Literal (IntNum 9))))
        , example "whitespace" "7 + 8 <<>> 9" $ at 1 1 1 13 (Binop (Raw "<<>>") (at 1 1 1 6 (Binop (Raw "+") (at 1 1 1 2 (Literal (IntNum 7))) (at 1 5 1 6 (Literal (IntNum 8))))) (at 1 12 1 13 (Literal (IntNum 9))))
        , example "comments" "7{-A-}+{-B-}8{-C-}<<>>{-D-}9" $ at 1 1 1 29 (Binop (Raw "<<>>") (at 1 1 1 14 (Binop (Raw "+") (at 1 1 1 2 (Literal (IntNum 7))) (at 1 13 1 14 (Literal (IntNum 8))))) (at 1 28 1 29 (Literal (IntNum 9))))
        , example "newlines" "7\n +\n 8\n <<>>\n 9" $ at 1 1 5 3 (Binop (Raw "<<>>") (at 1 1 3 3 (Binop (Raw "+") (at 1 1 1 2 (Literal (IntNum 7))) (at 3 2 3 3 (Literal (IntNum 8))))) (at 5 2 5 3 (Literal (IntNum 9))))
        ]

    , testGroup "parentheses"
        [ example "" "(1)" $ at 1 2 1 3 (Literal (IntNum 1))
        , example "whitespace" "( 1 )" $ at 1 3 1 4 (Literal (IntNum 1))
        , example "comments" "({-A-}1{-B-})" $ at 1 7 1 8 (Literal (IntNum 1))
        , example "newlines" "(\n 1\n )" $ at 2 2 2 3 (Literal (IntNum 1))
        ]

    , testGroup "List"
        [ example "" "[1,2,3]" $ at 1 1 1 8 (ExplicitList [at 1 2 1 3 (Literal (IntNum 1)),at 1 4 1 5 (Literal (IntNum 2)),at 1 6 1 7 (Literal (IntNum 3))])
        , example "single element" "[1]" $ at 1 1 1 4 (ExplicitList [at 1 2 1 3 (Literal (IntNum 1))])
        , example "empty" "[]" $ at 1 1 1 3 (ExplicitList [])
        , example "whitespace" "[ 1 , 2 , 3 ]" $ at 1 1 1 14 (ExplicitList [at 1 3 1 4 (Literal (IntNum 1)),at 1 7 1 8 (Literal (IntNum 2)),at 1 11 1 12 (Literal (IntNum 3))])
        , example "comments" "[{-A-}1{-B-},{-C-}2{-D-},{-E-}3{-F-}]" $ at 1 1 1 38 (ExplicitList [at 1 7 1 8 (Literal (IntNum 1)),at 1 19 1 20 (Literal (IntNum 2)),at 1 31 1 32 (Literal (IntNum 3))])
        , example "newlines" "[\n 1\n ,\n 2\n ,\n 3\n ]" $ at 1 1 7 3 (ExplicitList [at 2 2 2 3 (Literal (IntNum 1)),at 4 2 4 3 (Literal (IntNum 2)),at 6 2 6 3 (Literal (IntNum 3))])
        ]

    , testGroup "Range"
        [ example "" "[7..9]" $ at 1 1 1 7 (Range (at 1 2 1 3 (Literal (IntNum 7))) (at 1 5 1 6 (Literal (IntNum 9))))
        , example "whitespace" "[ 7 .. 9 ]" $ at 1 1 1 11 (Range (at 1 3 1 4 (Literal (IntNum 7))) (at 1 8 1 9 (Literal (IntNum 9))))
        , example "comments" "[{-A-}7{-B-}..{-C-}9{-D-}]" $ at 1 1 1 27 (Range (at 1 7 1 8 (Literal (IntNum 7))) (at 1 20 1 21 (Literal (IntNum 9))))
        , example "newlines" "[\n 7\n ..\n 9\n ]" $ at 1 1 5 3 (Range (at 2 2 2 3 (Literal (IntNum 7))) (at 4 2 4 3 (Literal (IntNum 9))))
        , testCase "does not allow whitespace between the dots" $
            assertFailure expr "[ 7 . . 9 ]"
        ]

    , testGroup "Tuple"
        [ example "" "(1,2)" $ at 1 1 1 6 (Data "_Tuple2" [at 1 2 1 3 (Literal (IntNum 1)),at 1 4 1 5 (Literal (IntNum 2))])
        , example "whitespace" "( 1 , 2 )" $ at 1 1 1 10 (Data "_Tuple2" [at 1 3 1 4 (Literal (IntNum 1)),at 1 7 1 8 (Literal (IntNum 2))])
        , example "comments" "({-A-}1{-B-},{-C-}2{-D-})" $ at 1 1 1 26 (Data "_Tuple2" [at 1 7 1 8 (Literal (IntNum 1)),at 1 19 1 20 (Literal (IntNum 2))])
        , example "newlines" "(\n 1\n ,\n 2\n )" $ at 1 1 5 3 (Data "_Tuple2" [at 2 2 2 3 (Literal (IntNum 1)),at 4 2 4 3 (Literal (IntNum 2))])
        ]

    , testGroup "tuple constructor"
        [ example "" "(,,)" $ at 1 1 1 5 (Lambda (at 1 1 1 5 (P.Var "v0")) (at 1 1 1 5 (Lambda (at 1 1 1 5 (P.Var "v1")) (at 1 1 1 5 (Lambda (at 1 1 1 5 (P.Var "v2")) (at 1 1 1 5 (Data "_Tuple3" [at 1 1 1 5 (Var (Raw "v0")),at 1 1 1 5 (Var (Raw "v1")),at 1 1 1 5 (Var (Raw "v2"))])))))))
        , example "whitespace" "( ,, )" $ at 1 1 1 7 (Lambda (at 1 1 1 7 (P.Var "v0")) (at 1 1 1 7 (Lambda (at 1 1 1 7 (P.Var "v1")) (at 1 1 1 7 (Lambda (at 1 1 1 7 (P.Var "v2")) (at 1 1 1 7 (Data "_Tuple3" [at 1 1 1 7 (Var (Raw "v0")),at 1 1 1 7 (Var (Raw "v1")),at 1 1 1 7 (Var (Raw "v2"))])))))))
        , example "comments" "({-A-},,{-B-})" $ at 1 1 1 15 (Lambda (at 1 1 1 15 (P.Var "v0")) (at 1 1 1 15 (Lambda (at 1 1 1 15 (P.Var "v1")) (at 1 1 1 15 (Lambda (at 1 1 1 15 (P.Var "v2")) (at 1 1 1 15 (Data "_Tuple3" [at 1 1 1 15 (Var (Raw "v0")),at 1 1 1 15 (Var (Raw "v1")),at 1 1 1 15 (Var (Raw "v2"))])))))))
        , example "newlines" "(\n ,,\n )" $ at 1 1 3 3 (Lambda (at 1 1 3 3 (P.Var "v0")) (at 1 1 3 3 (Lambda (at 1 1 3 3 (P.Var "v1")) (at 1 1 3 3 (Lambda (at 1 1 3 3 (P.Var "v2")) (at 1 1 3 3 (Data "_Tuple3" [at 1 1 3 3 (Var (Raw "v0")),at 1 1 3 3 (Var (Raw "v1")),at 1 1 3 3 (Var (Raw "v2"))])))))))
        , testCase "does not allow whitespace between the commas" $
            assertFailure expr "(, ,)"
        ]

    , testGroup "Record"
        [ testGroup "empty"
            [ example "" "{}" $ at 1 1 1 3 (Record [])
            , example "whitespace" "{ }" $ at 1 1 1 4 (Record [])
            , example "comments" "{{-A-}}" $ at 1 1 1 8 (Record [])
            ]

        , example "" "{x=7,y=8}" $ at 1 1 1 10 (Record [("x",at 1 4 1 5 (Literal (IntNum 7))),("y",at 1 8 1 9 (Literal (IntNum 8)))])
        , example "single field" "{x=7}" $ at 1 1 1 6 (Record [("x",at 1 4 1 5 (Literal (IntNum 7)))])
        , example "whitespace" "{ x = 7 , y = 8 }" $ at 1 1 1 18 (Record [("x",at 1 7 1 8 (Literal (IntNum 7))),("y",at 1 15 1 16 (Literal (IntNum 8)))])
        , example "comments" "{{-A-}x{-B-}={-C-}7{-D-},{-E-}y{-F-}={-G-}8{-H-}}" $ at 1 1 1 50 (Record [("x",at 1 19 1 20 (Literal (IntNum 7))),("y",at 1 43 1 44 (Literal (IntNum 8)))])
        , example "single field with comments" "{{-A-}x{-B-}={-C-}7{-D-}}" $ at 1 1 1 26 (Record [("x",at 1 19 1 20 (Literal (IntNum 7)))])
        , example "newlines" "{\n x\n =\n 7\n ,\n y\n =\n 8\n }" $ at 1 1 9 3 (Record [("x",at 4 2 4 3 (Literal (IntNum 7))),("y",at 8 2 8 3 (Literal (IntNum 8)))])
        ]

    , testGroup "Record update"
        [ example "" "{a|x=7,y=8}" $ at 1 1 1 12 (Update (at 1 2 1 3 (Var (Raw "a"))) [("x",at 1 6 1 7 (Literal (IntNum 7))),("y",at 1 10 1 11 (Literal (IntNum 8)))])
        , example "single field" "{a|x=7}" $ at 1 1 1 8 (Update (at 1 2 1 3 (Var (Raw "a"))) [("x",at 1 6 1 7 (Literal (IntNum 7)))])
        , example "whitespace" "{ a | x = 7 , y = 8 }" $ at 1 1 1 22 (Update (at 1 3 1 4 (Var (Raw "a"))) [("x",at 1 11 1 12 (Literal (IntNum 7))),("y",at 1 19 1 20 (Literal (IntNum 8)))])
        , example "comments" "{{-A-}a{-B-}|{-C-}x{-D-}={-E-}7{-F-},{-G-}y{-H-}={-I-}8{-J-}}" $ at 1 1 1 62 (Update (at 1 7 1 8 (Var (Raw "a"))) [("x",at 1 31 1 32 (Literal (IntNum 7))),("y",at 1 55 1 56 (Literal (IntNum 8)))])
        , example "newlines" "{\n a\n |\n x\n =\n 7\n ,\n y\n =\n 8\n }" $ at 1 1 11 3 (Update (at 2 2 2 3 (Var (Raw "a"))) [("x",at 6 2 6 3 (Literal (IntNum 7))),("y",at 10 2 10 3 (Literal (IntNum 8)))])
        , testCase "only allows simple base" $
            assertFailure expr "{9|x=7}"
        , testCase "only allows simple base" $
            assertFailure expr "{{}|x=7}"
        , testCase "must have fields" $
            assertFailure expr "{a|}"
        ]

    , testGroup "record access"
        [ example "" "x.f1" $ at 1 1 1 5 (Access (at 1 1 1 2 (Var (Raw "x"))) "f1")
        , example "nested" "x.f1.f2" $ at 1 3 1 8 (Access (at 1 1 1 5 (Access (at 1 1 1 2 (Var (Raw "x"))) "f1")) "f2")
        , testCase "does not allow symbolic field names" $
            assertFailure expr "x.+"
        , testCase "does not allow symbolic field names" $
            assertFailure expr "x.(+)"
        ]

    , testGroup "record access fuction"
        [ example "" ".f1" $ at 1 1 1 4 (Lambda (at 1 1 1 4 (P.Var "_")) (at 1 1 1 4 (Access (at 1 1 1 4 (Var (Raw "_"))) "f1")))
        ]

    , testGroup "lambda"
        [ example "" "\\x y->9" $ at 1 1 1 8 (Lambda (at 1 2 1 3 (P.Var "x")) (at 1 1 1 8 (Lambda (at 1 4 1 5 (P.Var "y")) (at 1 7 1 8 (Literal (IntNum 9))))))
        , example "single parameter" "\\x->9" $ at 1 1 1 6 (Lambda (at 1 2 1 3 (P.Var "x")) (at 1 5 1 6 (Literal (IntNum 9))))
        , example "whitespace" "\\ x y -> 9" $ at 1 1 1 11 (Lambda (at 1 3 1 4 (P.Var "x")) (at 1 1 1 11 (Lambda (at 1 5 1 6 (P.Var "y")) (at 1 10 1 11 (Literal (IntNum 9))))))
        , example "comments" "\\{-A-}x{-B-}y{-C-}->{-D-}9" $ at 1 1 1 27 (Lambda (at 1 7 1 8 (P.Var "x")) (at 1 1 1 27 (Lambda (at 1 13 1 14 (P.Var "y")) (at 1 26 1 27 (Literal (IntNum 9))))))
        , example "newlines" "\\\n x\n y\n ->\n 9" $ at 1 1 5 3 (Lambda (at 2 2 2 3 (P.Var "x")) (at 1 1 5 3 (Lambda (at 3 2 3 3 (P.Var "y")) (at 5 2 5 3 (Literal (IntNum 9))))))
        , testCase "arrow must not contain whitespace" $
            assertFailure expr "\\x y - > 9"
        , testCase "must have parameters" $
            assertFailure expr "\\ -> 9"
        ]

    , testGroup "if statement"
        [ example "" "if x then y else z" $ at 1 1 1 19 (If [(at 1 4 1 5 (Var (Raw "x")),at 1 11 1 12 (Var (Raw "y")))] (at 1 18 1 19 (Var (Raw "z"))))
        , example "comments" "if{-A-}x{-B-}then{-C-}y{-D-}else{-E-}z" $ at 1 1 1 39 (If [(at 1 8 1 9 (Var (Raw "x")),at 1 23 1 24 (Var (Raw "y")))] (at 1 38 1 39 (Var (Raw "z"))))
        , example "else if" "if x then y else if x' then y' else if x'' then y'' else z" $ at 1 1 1 59 (If [(at 1 4 1 5 (Var (Raw "x")),at 1 11 1 12 (Var (Raw "y"))),(at 1 21 1 23 (Var (Raw "x'")),at 1 29 1 31 (Var (Raw "y'"))),(at 1 40 1 43 (Var (Raw "x''")),at 1 49 1 52 (Var (Raw "y''")))] (at 1 58 1 59 (Var (Raw "z"))))
        , example "newlines" "if\n x\n then\n y\n else\n z" $ at 1 1 6 3 (If [(at 2 2 2 3 (Var (Raw "x")),at 4 2 4 3 (Var (Raw "y")))] (at 6 2 6 3 (Var (Raw "z"))))
        ]

    , testGroup "let statement"
        [ example "" "let a=b in z" $ at 1 1 1 13 (Let [at 1 5 1 8 (Definition (at 1 5 1 6 (P.Var "a")) (at 1 7 1 8 (Var (Raw "b"))))] (at 1 12 1 13 (Var (Raw "z"))))
        , example "multiple declarations" "let a=b\n    c=d\n in z" $ at 1 1 3 6 (Let [at 1 5 1 8 (Definition (at 1 5 1 6 (P.Var "a")) (at 1 7 1 8 (Var (Raw "b")))),at 2 5 2 8 (Definition (at 2 5 2 6 (P.Var "c")) (at 2 7 2 8 (Var (Raw "d"))))] (at 3 5 3 6 (Var (Raw "z"))))
        , example "multiple declarations (newline after let)" "let\n a=b\n c=d\n in z" $ at 1 1 4 6 (Let [at 2 2 2 5 (Definition (at 2 2 2 3 (P.Var "a")) (at 2 4 2 5 (Var (Raw "b")))),at 3 2 3 5 (Definition (at 3 2 3 3 (P.Var "c")) (at 3 4 3 5 (Var (Raw "d"))))] (at 4 5 4 6 (Var (Raw "z"))))
        , example "whitespace" "let a = b in z" $ at 1 1 1 15 (Let [at 1 5 1 10 (Definition (at 1 5 1 6 (P.Var "a")) (at 1 9 1 10 (Var (Raw "b"))))] (at 1 14 1 15 (Var (Raw "z"))))
        , example "comments" "let{-A-}a{-B-}={-C-}b{-D-}in{-E-}z" $ at 1 1 1 35 (Let [at 1 9 1 22 (Definition (at 1 9 1 10 (P.Var "a")) (at 1 21 1 22 (Var (Raw "b"))))] (at 1 34 1 35 (Var (Raw "z"))))
        , example "newlines" "let\n a\n =\n b\n in\n z" $ at 1 1 6 3 (Let [at 2 2 4 3 (Definition (at 2 2 2 3 (P.Var "a")) (at 4 2 4 3 (Var (Raw "b"))))] (at 6 2 6 3 (Var (Raw "z"))))
        , testCase "must have at least one definition" $
            assertFailure expr "let in z"
        , testGroup "declarations must start at the same column" $
            [ testCase "(1)" $ assertFailure expr "let a=b\n   c=d\nin z"
            , testCase "(2)" $ assertFailure expr "let a=b\n     c=d\nin z"
            , testCase "(3)" $ assertFailure expr "let  a=b\n   c=d\nin z"
            ]
        ]

    , testGroup "case statement"
        [ example "" "case 9 of\n 1->10\n _->20" $ at 1 1 3 7 (Case (at 1 6 1 7 (Literal (IntNum 9))) [(at 2 2 2 3 (P.Literal (IntNum 1)),at 2 5 2 7 (Literal (IntNum 10))),(at 3 2 3 3 Anything,at 3 5 3 7 (Literal (IntNum 20)))])
        , example "no newline after 'of'" "case 9 of 1->10\n          _->20" $ at 1 1 2 16 (Case (at 1 6 1 7 (Literal (IntNum 9))) [(at 1 11 1 12 (P.Literal (IntNum 1)),at 1 14 1 16 (Literal (IntNum 10))),(at 2 11 2 12 Anything,at 2 14 2 16 (Literal (IntNum 20)))])
        , example "whitespace" "case 9 of\n 1 -> 10\n _ -> 20" $ at 1 1 3 9 (Case (at 1 6 1 7 (Literal (IntNum 9))) [(at 2 2 2 3 (P.Literal (IntNum 1)),at 2 7 2 9 (Literal (IntNum 10))),(at 3 2 3 3 Anything,at 3 7 3 9 (Literal (IntNum 20)))])

        , example "comments" "case{-A-}9{-B-}of{-C-}\n{-D-}1{-E-}->{-F-}10{-G-}\n{-H-}_{-I-}->{-J-}20" $ at 1 1 3 21 (Case (at 1 10 1 11 (Literal (IntNum 9))) [(at 2 6 2 7 (P.Literal (IntNum 1)),at 2 19 2 21 (Literal (IntNum 10))),(at 3 6 3 7 Anything,at 3 19 3 21 (Literal (IntNum 20)))])
        , example "newlines" "case\n 9\n of\n 1\n ->\n 10\n _\n ->\n 20" $ at 1 1 9 4 (Case (at 2 2 2 3 (Literal (IntNum 9))) [(at 4 2 4 3 (P.Literal (IntNum 1)),at 6 2 6 4 (Literal (IntNum 10))),(at 7 2 7 3 Anything,at 9 2 9 4 (Literal (IntNum 20)))])
        , testCase "should not consume trailing whitespace" $
            assertParse (expr >> string "\nX") "case 9 of\n 1->10\n _->20\nX" $ "\nX"
        , testGroup "clauses must start at the same column"
            [ testCase "(1)" $ assertFailure expr "case 9 of\n 1->10\n_->20"
            , testCase "(2)" $ assertFailure expr "case 9 of\n 1->10\n  _->20"
            , testCase "(3)" $ assertFailure expr "case 9 of\n  1->10\n _->20"
            ]
        ]
    ]
