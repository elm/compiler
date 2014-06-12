module Debug where
{-| This library is for investigating bugs or performance problems. It should
*not* be used in production code.

@docs log, crash
-}

import Native.Debug
import Native.Error

{-| Log a tagged value on the developer console, and then return the value.

      1 + log "number" 1        -- equals 2, logs "number: 1"
      length (log "start" [])   -- equals 0, logs "start: []"

Notice that `log` is not a pure function! It should *only* be used for
investigating bugs or performance problems.
-}
log : String -> a -> a
log = Native.Debug.log

{-| Crash the program with an error message. This is an uncatchable error,
intended for code that is soon-to-be-implemented. For example, if you are
working with a large ADT and have partially completed a case expression, it may
make sense to do this:

      data Entity = Ship | Fish | Captain | Seagull

      drawEntity entity =
          case entity of
            Ship -> ...
            Fish -> ...
            _ -> Debug.crash ("drawEntity not implemented for " ++ show entity ++ " yet!")

Note that incomplete pattern matches are *very* bad practice! They are one of
the very few ways to crash an Elm program, and they are completely avoidable.
Production code should not have incomplete pattern matches!

**Use this if** you want to do some testing while you are partway through
writing a function.

**Do not use this if** you want to do some typical try-catch exception handling.
Use the `Maybe` or `Either` libraries instead.
-}
crash : String -> a
crash = Native.Error.raise