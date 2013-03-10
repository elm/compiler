
module Json where

import Dict
import JavaScript as JS
import Native.Json as Native


data Value
  = String String
  | Number Float
  | Boolean Bool
  | Null
  | Array [Value]
  | Object (Dict String Value)


-- String Converters

toString : Value -> String
toString v = JS.toString (Native.toPrettyJSString "" v)

toPrettyString : String -> Value -> String
toPrettyString sep v = JS.toString (Native.toPrettyJSString sep v)

toJSString : Value -> JSString
toJSString v = Native.toPrettyJSString "" v

fromString : String -> Maybe Value
fromString s = Native.fromJSString (JS.fromString s)

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

find get base =
  let f key dict =
          case Dict.lookup key dict of
            Nothing -> base
            Just v  -> get v
  in  f

findString : String -> Dict String Value -> String
findString = find string ""

findNumber : String -> Dict String Value -> Float
findNumber = find number 0

findBoolean : String -> Dict String Value -> Bool
findBoolean = find boolean False

findArray : String -> Dict String Value -> [Value]
findArray = find array []

findObject : String -> Dict String Value -> Dict String Value
findObject = find object Dict.empty
