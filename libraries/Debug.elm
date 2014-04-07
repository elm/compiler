module Debug where
{-| This library is for investigating bugs or performance problems. It should
*not* be used in production code.

@docs log, trace
-}

import Graphics.Element (Element)
import Native.Debug

{-| Log a tagged value on the developer console, and then return the value.

      1 + log "number" 1        -- equals 2, logs "number: 1"
      length (log "start" [])   -- equals 0, logs "start: []"

Notice that `log` is not a pure function! It should *only* be used for
investigating bugs or performance problems.
-}
log : String -> a -> a
log = Native.Debug.log

{-| Watch a particular value in the reactive debugger.
-}
watch : String -> a -> a
watch = Native.Debug.watch

{-| Watch a particular value in the reactive debugger.
-}
watchSummary : String -> (a -> b) -> a -> a
watchSummary = Native.Debug.watchSummary

{-| Trace an Element over time in the reactive debugger.
-}
trace : String -> Element -> Element
trace = Native.Debug.tracePath

