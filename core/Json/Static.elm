
module Json.Static where

import JavaScript as JS
import Native.Json as Native

fromJSString = Native.recordFromJSString
fromString s = Native.recordFromJSString (JS.fromString s)

toString r = JS.toString (Native.recordToPrettyJSString "" r)
toPrettyString sep r = JS.toString (Native.recordToPrettyJSString sep r)
toJSString r = Native.recordToPrettyJSString "" r