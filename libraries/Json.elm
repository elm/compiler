
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
fromJSString = Native.fromJSString


-- Extract Elm values from Json values

string : String -> Value -> String
string base v = case v of { String s -> s ; _ -> base }

number : Float -> Value -> Float
number base v = case v of { Number n -> n ; _ -> base }

boolean : Bool -> Value -> Bool
boolean base v = case v of { Boolean b -> b ; _ -> base }

array : [Value] -> Value -> [Value]
array base v = case v of { Array a -> a ; _ -> base }

object : Dict String Value -> Value -> Dict String Value
object base v = case v of { Object o -> o ; _ -> base }


-- Extract Elm values from dictionaries of Json values

find get =
  let f base key dict =
          case Dict.lookup key dict of
            Nothing -> base
            Just v  -> get base v
  in  f

findString : String -> Dict String Value -> String
findString = find string

findNumber : Float -> Dict String Value -> Float
findNumber = find number

findBoolean : Bool -> Dict String Value -> Bool
findBoolean = find boolean

findArray : [Value] -> Dict String Value -> [Value]
findArray = find array

findObject : Dict String Value -> Dict String Value -> Dict String Value
findObject = find object
