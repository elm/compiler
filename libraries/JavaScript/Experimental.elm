
module JavaScript.Experimental where

import JavaScript as JS

toRecord : JSObject -> a
toRecord = JS.toRecord

fromRecord : a -> JSObject
fromRecord = JS.fromRecord