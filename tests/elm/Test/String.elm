module Test.String (tests) where

import List
import String (..)

import ElmTest.Assertion (..)
import ElmTest.Test (..)

tests : Test
tests =
  let simpleTests = suite "Simple Stuff"
        [ test "is empty" <| assert (isEmpty "")
        , test "is not empty" <| assert (not (isEmpty ("the world")))
        , test "length" <| assertEqual 11 (length "innumerable")
        , test "endsWith" (assert <| endsWith "ship" "spaceship")
        , test "reverse" <| assertEqual "desserts" (reverse "stressed")
        , test "repeat" <| assertEqual "hahaha" (repeat 3 "ha")
        ]

      combiningTests = suite "Combining Strings"
        [ test "uncons non-empty" <| assertEqual (Just ('a',"bc")) (uncons "abc")
        , test "uncons empty" <| assertEqual Nothing (uncons "")
        , test "append 1" <| assertEqual "butterfly" (append "butter" "fly")
        , test "append 2" <| assertEqual "butter" (append "butter" "")
        , test "append 3" <| assertEqual "butter" (append "" "butter")
        , test "concat" <| assertEqual "nevertheless" (concat ["never","the","less"])
        , test "split commas" <| assertEqual ["cat","dog","cow"] (split "," "cat,dog,cow")
        , test "split slashes"<| assertEqual ["home","steve","Desktop", ""] (split "/" "home/steve/Desktop/")
        , test "join spaces"  <| assertEqual "cat dog cow" (join " " ["cat","dog","cow"])
        , test "join slashes" <| assertEqual "home/steve/Desktop" (join "/" ["home","steve","Desktop"])
        ]
  in
      suite "String" [ simpleTests, combiningTests ]