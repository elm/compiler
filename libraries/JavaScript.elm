
module JavaScript where


-- Requires that the input array be uniform (all members have the same type)
toList : JSArray a -> [a]
toInt : JSNumber -> Int
toFloat : JSNumber -> Float
toBool : JSBool -> Bool
toString : JSString -> String


-- Produces a uniform JavaScript array with all members of the same type.
fromList : [a] -> JSArray a
fromInt : Int -> JSNumber
fromFloat : Float -> JSNumber
fromBool : Bool -> JSBool
fromString : String -> JSString


-- Turn an `Element` into a plain old DOM node.
fromElement : Element -> JSDomNode

-- Turn a DOM node into an `Element`. You can resize the node
-- using the normal `width` and `height` functions.
toElement : Int -> Int -> JSDomNode -> Element
