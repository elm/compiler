
module JavaScript where

import Native.JavaScript
import open Basics

data JSNumber = JSNumber
data JSBool = JSBool
data JSString = JSString
data JSArray a = JSArray a
data JSDomNode = JSDomNode
data JSObject = JSObject

-- Requires that the input array be uniform (all members have the same type)
toList : JSArray a -> [a]
toList = Native.JavaScript.toList

toInt : JSNumber -> Int
toInt = Native.JavaScript.toInt

toFloat : JSNumber -> Float
toFloat = Native.JavaScript.toFloat

toBool : JSBool -> Bool
toBool = Native.JavaScript.toBool

toString : JSString -> String
toString = Native.JavaScript.toString


-- Produces a uniform JavaScript array with all members of the same type.
fromList : [a] -> JSArray a
fromList = Native.JavaScript.fromList

fromInt : Int -> JSNumber
fromInt = Native.JavaScript.fromInt

fromFloat : Float -> JSNumber
fromFloat = Native.JavaScript.fromFloat

fromBool : Bool -> JSBool
fromBool = Native.JavaScript.fromBool

fromString : String -> JSString
fromString = Native.JavaScript.fromString

{--
-- Turn an `Element` into a plain old DOM node.
fromElement : Element -> JSDomNode
fromElement = Native.JavaScript.fromElement

-- Turn a DOM node into an `Element`. You can resize the node
-- using the normal `width` and `height` functions.
toElement : Int -> Int -> JSDomNode -> Element
toElement = Native.JavaScript.toElement
--}