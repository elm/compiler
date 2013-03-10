
module JavaScript where


-- To Elm

toString : JSString -> String
toList : JSArray a -> [a]
toInt : JSNumber -> Int
toFloat : JSNumber -> Float
toBool : JSBool -> Bool


-- From Elm

fromString : String -> JSString
fromList : [a] -> JSArray a
fromInt : Int -> JSNumber
fromFloat : Float -> JSNumber
fromBool : Bool -> JSBool

