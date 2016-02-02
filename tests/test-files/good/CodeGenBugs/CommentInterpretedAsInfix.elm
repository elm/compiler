
-- Issue: https://github.com/elm-lang/elm-compiler/issues/894
-- Two dashes immediately follow by other symbols are incorrectly
-- interpreted as an infix operator, instead of a comment.

(|>) a f = f a

always x s = x 

value = "Hi"
    |> always "Hello"
    --|> always "Bye"