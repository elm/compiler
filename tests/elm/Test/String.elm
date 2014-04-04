module Test.String (tests) where

import List
import String (..)

import ElmTest.Assertion (..)
import ElmTest.Test (..)

tests : [Test]
tests =
    let isEmptyTests = [test "empty is empty" <| assert <| isEmpty "",
                        test "not empty" <| assert <|  not . isEmpty <| "the world"]
        lengthTest = test "length of innumerable" <| assertEqual 11 (length "innumerable")
        reverseTest = test "reverse test" <| assertEqual "desserts" (reverse "stressed")
        repeatTest = test "repeat test" <| assertEqual "hahaha" (repeat 3 "ha")
        unconsTests = [test "nonempty uncons" <| assertEqual (Just ('a',"bc")) (uncons "abc"),
                       test "empty uncons"    <| assertEqual Nothing           (uncons "")]
        appendTest = test "append test" <| assertEqual "butterfly"    (append "butter" "fly")
        concatTest = test "concat test" <| assertEqual "nevertheless" (concat ["never","the","less"])
        splitTests = [test "split commas" <| assertEqual ["cat","dog","cow"] (split "," "cat,dog,cow"),
                     test "split slashes"<| assertEqual ["home","evan","Desktop", ""] (split "/" "home/evan/Desktop/")]
        joinTests = [test "join spaces"  <| assertEqual "cat dog cow" (join " " ["cat","dog","cow"]),
                     test "join slashes" <| assertEqual "home/evan/Desktop" (join "/" ["home","evan","Desktop"])]
        
        endsWithTest = test "endsWith Check" (assert <| endsWith "h" "th")
    in List.concat [isEmptyTests, [lengthTest, reverseTest, repeatTest], unconsTests, [appendTest, concatTest], splitTests, joinTests, [endsWithTest]]