
module Json.Static where

import JavaScript as JS
import Native.Json as Native

fromString s = Native.recordFromJSString (JS.fromString s)
fromJSString = Native.recordFromJSString

toString r = JS.toString (Native.recordToPrettyJSString "" r)
toPrettyString sep r = JS.toString (Native.recordToPrettyJSString sep r)
toJSString r = Native.recordToPrettyJSString "" r