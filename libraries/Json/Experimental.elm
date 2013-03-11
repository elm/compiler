
module Json.Experimental where

import JavaScript as JS
import Native.Json (recordFromJSString, recordToPrettyJSString)

fromString s = recordFromJSString (JS.fromString s)
fromJSString = recordFromJSString

toString r = JS.toString (recordToPrettyJSString "" r)
toPrettyString sep r = JS.toString (recordToPrettyJSString sep r)
toJSString r = recordToPrettyJSString "" r