
module JavaScript where

import Native.JavaScript as JS

-- To Elm

toString : JSString -> String
toString = JS.toString

toInt : JSNumber -> Int
toInt = JS.toInt

toFloat : JSNumber -> Float
toFloat = JS.id

toBool : JSBool -> Bool
toBool = JS.id

toList : JSArray a -> [a]
toList = JS.toList

toRecord = JS.toRecord


-- From Elm

fromString : String -> JSString
fromString = JS.fromString

fromInt : Int -> JSNumber
fromInt = JS.id

fromFloat : Float -> JSNumber
fromFloat = JS.id

fromBool : Bool -> JSBool
fromBool = JS.id

fromList : [a] -> JSArray a
fromList = JS.fromList

fromRecord = JS.fromRecord
