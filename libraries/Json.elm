
module Json where

{-| Library for working with [JSON](https://en.wikipedia.org/wiki/JSON) values.

# Json Values
@docs JsonValue

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
object. In Elm, a proper JSON object is represented as a (Dict String JsonValue)
which is a mapping from strings to Json Values. -}
data JsonValue
    = String String
    | Number Float
    | Boolean Bool
    | Null
    | Array [JsonValue]
    | Object (Dict.Dict String JsonValue)


-- String Converters

{-| Convert a `JsonValue` into a prettified string.
The first argument is a separator token (e.g. \" \", \"\\n\", etc.) that will
be used for indentation in the prettified string version of the JSON. -}
toString : String -> JsonValue -> String
toString sep v = JS.toString (Native.Json.toJSString sep v)

{-| Convert a proper JSON object into a JavaScript string.
Note that the type JSString seen here is not the same as the type constructor
JsonString used elsewhere in this module. -}
toJSString : String -> JsonValue -> JSString
toJSString = Native.Json.toJSString

{-| Parse a string representation of a proper JSON object into
its Elm representation. -}
fromString : String -> Maybe JsonValue
fromString s = Native.Json.fromJSString (JS.fromString s)

{-| Parse a JavaScript string representation of a proper JSON object into
its Elm representation. -}
fromJSString : JSString -> Maybe JsonValue
fromJSString = Native.Json.fromJSString

{-| Convert a JS object into a `JsonValue`. -}
fromJSObject : JSObject -> JsonValue
fromJSObject = Native.Json.fromJSObject

{-| Convert a `JsonValue` into a `JSObject`. Paired with the
[`JavaScript.Experimental` library](/docs/JavaScript/Experimental.elm),
This lets you convert strings into Elm records:

       import JavaScript.Experimental as JS

       stringToRecord str =
           case fromString str of
             Just jsonValue -> Just (JS.toRecord (toJSObject jsonValue))
             Nothing -> Nothing
-}
toJSObject : JsonValue -> JSObject
toJSObject = Native.Json.toJSObject

 {-- Extract Elm values from Json values

 string : JsonValue -> String
 string v = case v of { String s -> s ; _ -> "" }

 number : JsonValue -> Float
 number v = case v of { Number n -> n ; _ -> 0 }

 boolean : JsonValue -> Bool
 boolean v = case v of { Boolean b -> b ; _ -> False }

 array : JsonValue -> [JsonValue]
 array v = case v of { Array a -> a ; _ -> [] }

 object : JsonValue -> Dict String JsonValue
 object v = case v of { Object o -> o ; _ -> Dict.empty }


 -- Extract Elm values from dictionaries of Json values

 {-| Find a value in a Json Object using the passed get function. If the key is
 not found, this returns the given default/base value. -}
 find get base =
   let f key dict =
           case Dict.lookup key dict of
             Nothing -> base
             Just v  -> get v
   in  f

 {-| Find a string value in an Elm Json object. If the key is not found or the
 value found is not a string, this returns the empty string. -}
 findString : String -> Object -> String
 findString = find string ""

 {-| Find a number value in an Elm Json object. If the key is not found or the
 value found is not a number, this returns 0 -}
 findNumber : String -> Object -> Float
 findNumber = find number 0

 {-| Find a boolean value in an Elm Json object. If the key is not found or the
 value found is not a boolean, this returns the False. -}
 findBoolean : String -> Object -> Bool
 findBoolean = find boolean False

 {-| Find an array value in an Elm Json object. If the key is not found or the
 value found is not an array, this returns an empty list. -}
 findArray : String -> Object -> [JsonValue]
 findArray = find array []

 {-| Find an object value in an Elm Json object. If the key is not found or the
 value found is not an object, this returns an empty object. -}
 findObject : String -> Object -> Object
 findObject = find object Dict.empty
 --}
