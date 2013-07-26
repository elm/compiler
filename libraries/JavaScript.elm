
module JavaScript where

import Native.JavaScript as Native

data JSNumber = JSNumber
data JSBool = JSBool
data JSString = JSString
data JSArray a = JSArray a
data JSDomNode = JSDomNode
data JSObject = JSObject

-- Requires that the input array be uniform (all members have the same type)
toList : JSArray a -> [a]
toList = Native.toList

toInt : JSNumber -> Int
toInt = Native.toInt

toFloat : JSNumber -> Float
toFloat = Native.toFloat

toBool : JSBool -> Bool
toBool = Native.toBool

toString : JSString -> String
toString = Native.toString


-- Produces a uniform JavaScript array with all members of the same type.
fromList : [a] -> JSArray a
fromList = Native.fromList

fromInt : Int -> JSNumber
fromInt = Native.fromInt

fromFloat : Float -> JSNumber
fromFloat = Native.fromFloat

fromBool : Bool -> JSBool
fromBool = Native.fromBool

fromString : String -> JSString
fromString = Native.fromString


-- Turn an `Element` into a plain old DOM node.
fromElement : Element -> JSDomNode
fromElement = Native.fromElement

-- Turn a DOM node into an `Element`. You can resize the node
-- using the normal `width` and `height` functions.
toElement : Int -> Int -> JSDomNode -> Element
toElement = Native.toElement

