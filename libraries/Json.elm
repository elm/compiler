
-- TOOD: Evan please review texts

module Json where

import Dict as Dict
import JavaScript as JS
import Native.Json as Native

-- This datatype can represent all valid values that can be held in a JSON
-- object. In Elm, a proper JSON object is represented as a (Dict String Value)
-- which is a mapping from strings to Json Values.
data Value
  = String String
  | Number Float
  | Boolean Bool
  | Null
  | Array [Value]
  | Object (Dict String Value)


-- String Converters

-- Convert a proper JSON object into a string.
toString : Value -> String
toString v = JS.toString (Native.toPrettyJSString "" v)

-- Convert a proper JSON object into a prettified string.
-- The first argument is a separator token (e.g. \" \", \"\\n\", etc.) that will
-- be used for indentation in the prettified string version of the JSON object.
toPrettyString : String -> Value -> String
toPrettyString sep v = JS.toString (Native.toPrettyJSString sep v)

-- Convert a proper JSON object into a JavaScript string.
-- Note that the type JSString seen here is not the same as the type constructor
-- JsonString used elsewhere in this module.
toJSString : Value -> JSString
toJSString v = Native.toPrettyJSString "" v

-- Parse a string representation of a proper JSON object into
-- its Elm's representation.
fromString : String -> Maybe Value
fromString s = Native.fromJSString (JS.fromString s)

-- Parse a JavaScript string representation of a proper JSON object into
-- its Elm's representation.
fromJSString : JSString -> Maybe Value


-- Extract Elm values from Json values

string : Value -> String
string v = case v of { String s -> s ; _ -> "" }

number : Value -> Float
number v = case v of { Number n -> n ; _ -> 0 }

boolean : Value -> Bool
boolean v = case v of { Boolean b -> b ; _ -> False }

array : Value -> [Value]
array v = case v of { Array a -> a ; _ -> [] }

object : Value -> Dict String Value
object v = case v of { Object o -> o ; _ -> Dict.empty }


-- Extract Elm values from dictionaries of Json values

-- Find a value in a Json Object using the passed get function. If the key is
-- not found, this returns the given default/base value.
find get base =
  let f key dict =
          case Dict.lookup key dict of
            Nothing -> base
            Just v  -> get v
  in  f

-- Find a string value in an Elm Json object. If the key is not found or the
-- value found is not a string, this returns the empty string.
findString : String -> Object -> String
findString = find string ""

-- Find a number value in an Elm Json object. If the key is not found or the
-- value found is not a number, this returns 0
findNumber : String -> Object -> Float
findNumber = find number 0

-- Find a boolean value in an Elm Json object. If the key is not found or the
-- value found is not a boolean, this returns the False.
findBoolean : String -> Object -> Bool
findBoolean = find boolean False

-- Find an array value in an Elm Json object. If the key is not found or the
-- value found is not an array, this returns an empty list.
findArray : String -> Object -> [Value]
findArray = find array []

-- Find an object value in an Elm Json object. If the key is not found or the
-- value found is not an object, this returns an empty object.
findObject : String -> Object -> Object
findObject = find object Dict.empty
