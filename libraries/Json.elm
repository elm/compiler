
module Json where

{-| Library for working with [JSON](https://en.wikipedia.org/wiki/JSON) values.

@docs Value, fromString, toString

-}

import Dict
import Maybe (Maybe)
import Native.Json

{-| This datatype can represent all valid JSON values. A `Value` can be sent
through ports, and the runtime will do a conversion like this:

      [2,'abc',null] == Array [Number 2, String "abc", Null]

JSON is an intentional subset of JavaScript values which excludes functions.
This ensures that impure functions cannot sneak into Elm through ports.
-}
data Value
    = String String
    | Number Float
    | Boolean Bool
    | Null
    | Array [Value]
    | Object (Dict.Dict String Value)


{-| Convert a `Value` into a prettified string.
The first argument is a separator token (e.g. \" \", \"\\n\", etc.) that will
be used for indentation in the prettified string version of the JSON.

      toString "" Null         == "null"
      toString "" (Number 4.0) == "4.0"
-}
toString : String -> Value -> String
toString sep value = Native.Json.toString sep value

{-| Parse a string representation of a proper JSON object into
its Elm representation.

      fromString "null"      == Just Null
      fromString "[true,3]"  == Just (Array [Boolean True, Number 3])
      fromString "{'abc':4}" == Just (Object (Dict.fromList [("abc", Number 4)]))
-}
fromString : String -> Maybe Value
fromString str = Native.Json.fromString str
