module JavaScript.Experimental where
{-| A pair of very experimental functions for converting between
Elm records and JavaScript objects. It currently uses a best effort
conversion that can cause runtime errors. This API should change and
improve as we find its key failings.

# Converting between Elm records and JS Objects
@docs toRecord, fromRecord
-}

import JavaScript (JSObject)
import Native.JavaScript

{-| Turn arbitrary JavaScript objects into Elm records.
Arrays are converted into lists, nested objects are allowed.
No `null` values or non-homogeneous arrays.

        -- OK objects
        { student:"Steve", scores:[83,94,99,72] }
        { errorLevel:10, critical:true }

        -- BAD objects
        { answer:null }
        { info:[true,42,'what'] }
-}
toRecord : JSObject -> a
toRecord = Native.JavaScript.toRecord

{-| Turn arbitrary Elm records into JavaScript objects.
Lists become arrays, nested records are allowed. No ADTs.

        -- OK records
        { student="Steve", scores=[83,94,99,72] }
        { errorLevel=10, critical=True }

        -- BAD records
        { answer = Nothing }
        { result = Left "An error occurred" }
-}
fromRecord : a -> JSObject
fromRecord = Native.JavaScript.fromRecord