
module JavaScript where


-- To Elm

toString : JSString -> String

-- Requires that the input array be uniform (all members have the same type)
toList : JSArray a -> [a]

toInt : JSNumber -> Int
toFloat : JSNumber -> Float

-- Conversion from JavaScript boolean values to Elm boolean values.
toBool : JSBool -> Bool


-- From Elm

fromString : String -> JSString

-- Produces a uniform JavaScript array with all members of the same type.
fromList : [a] -> JSArray a

fromInt : Int -> JSNumber
fromFloat : Float -> JSNumber

-- Conversion from Elm boolean values to JavaScript boolean values.
fromBool : Bool -> JSBool

{-- TODO: only found in docs

  , ("castTupleToJSTuple2"    , "(a,b) -> JSTuple2 a b", "A JSTupleN is an array of size N with nonuniform types. Each member can have a different type.")
  , ("castJSTupleToTuple2"    , "JSTuple2 a b -> (a,b)", "")
  , ("castTupleToJSTuple3"    , "(a,b,c) -> JSTuple3 a b c", "")
  , ("castJSTupleToTuple3"    , "JSTuple3 a b c > (a,b,c)", "")
  , ("castTupleToJSTuple4"    , "(a,b,c,d) -> JSTuple4 a b c d", "")
  , ("castJSTupleToTuple4"    , "JSTuple4 a b c d -> (a,b,c,d)", "")
  , ("castTupleToJSTuple5"    , "(a,b,c,d,e) -> JSTuple5 a b c d e", "")
  , ("castJSTupleToTuple5"    , "JSTuple5 a b c d e -> (a,b,c,d,e)", "")

--}
