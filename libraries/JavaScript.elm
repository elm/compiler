
module JavaScript where

{-|This library does basic conversions between Elm and JS values. This allows
the internal data structures of Elm to change and improve with no impact on JS
integration.

It is intended to be imported as `import JavaScript as JS`. That way functions
like `JS.toInt` convert JavaScript *to* Elm integers and functions like
`JS.fromString` gets JavaScript *from* Elm strings.

# Javascript to Elm
@docs toString, toInt, toFloat, toBool, toList

# JavaScript from Elm
@docs fromString, fromInt, fromFloat, fromBool, fromList
-}
{-
# DOM Nodes and Elements
@docs toElement, fromElement
-}

import Native.JavaScript

data JSNumber = JSNumber
data JSBool = JSBool
data JSString = JSString
data JSArray a = JSArray a
data JSDomNode = JSDomNode
data JSObject = JSObject

{-| Requires that the input array be uniform (all members have the same type) -}
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


{-| Produces a uniform JavaScript array with all members of the same type. -}
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

{-
{-| Turn an `Element` into a DOM node. -}
fromElement : Element -> JSDomNode
fromElement = Native.JavaScript.fromElement

{-| Turn a DOM node into an `Element`. You can resize the node
using the normal `width` and `height` functions. -}
toElement : Int -> Int -> JSDomNode -> Element
toElement = Native.JavaScript.toElement
-}