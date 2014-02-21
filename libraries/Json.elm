
module Json where

{-| Library for working with [JSON](https://en.wikipedia.org/wiki/JSON) values.

# Json Values
@docs Json

# Strings
@docs fromString, toString

# JS Strings
@docs fromJSString, toJSString

# JS Objects
@docs fromJSObject, toJSObject

-}

import Basics (..)
import Dict
import Maybe (Maybe)
import JavaScript as JS
import Native.Json
import JavaScript (JSObject, JSString)

{-| This datatype can represent all valid values that can be held in a JSON
object. In Elm, a proper JSON object is represented as a (Dict String Json)
which is a mapping from strings to Json Values. -}
data Json
    = String String
    | Number Float
    | Boolean Bool
    | Null
    | Array [Json]
    | Object (Dict.Dict String Json)


-- String Converters

{-| Convert a `Json` into a prettified string.
The first argument is a separator token (e.g. \" \", \"\\n\", etc.) that will
be used for indentation in the prettified string version of the JSON. -}
toString : String -> Json -> String
toString sep v = JS.toString (Native.Json.toJSString sep v)

{-| Convert a proper JSON object into a JavaScript string.
Note that the type JSString seen here is not the same as the type constructor
JsonString used elsewhere in this module. -}
toJSString : String -> Json -> JSString
toJSString = Native.Json.toJSString

{-| Parse a string representation of a proper JSON object into
its Elm representation. -}
fromString : String -> Maybe Json
fromString s = Native.Json.fromJSString (JS.fromString s)

{-| Parse a JavaScript string representation of a proper JSON object into
its Elm representation. -}
fromJSString : JSString -> Maybe Json
fromJSString = Native.Json.fromJSString

{-| Convert a JS object into a `Json`. -}
fromJSObject : JSObject -> Json
fromJSObject = Native.Json.fromJSObject

{-| Convert a `Json` into a `JSObject`. Paired with the
[`JavaScript.Experimental` library](/docs/JavaScript/Experimental.elm),
This lets you convert strings into Elm records:

       import JavaScript.Experimental as JS

       stringToRecord str =
           case fromString str of
             Just json -> Just (JS.toRecord (toJSObject json))
             Nothing -> Nothing
-}
toJSObject : Json -> JSObject
toJSObject = Native.Json.toJSObject
