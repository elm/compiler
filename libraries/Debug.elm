module Debug where
{-| This library is for investigating bugs or performance problems. It should
*not* be used in production code.

# Console Debugging
@docs log, crash

# Time-Travel Debugging
@docs watch, watchSummary, trace
-}

import Graphics.Collage (Form)
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

{-| Watch a particular value in the debugger. Say we want to know the value of
a variable called `velocity` because it may not be updated correctly. Adding
`Debug.watch` allows us to name the value and show it with the debugger.

	  Debug.watch "velocity" velocity == velocity

Notice that the result of evaluating this expression is exactly the same as
not having the expression at all. That means it's easy to add to any value.
-}
watch : String -> a -> a
watch = Native.Debug.watch

{-| Watch a summary of a particular value in the debugger. This function is
pretty much the same as `watch` but it lets you specify a way to summarize
the value you are interested in. For example, maybe you only want to see part
of a record:

	  Debug.watchSummary "velocity" .velocity object

This is the same as just writing `object`, but it creates a watch that *only*
looks at the value of `object.velocity`. You can also show summary statistics
like length of a list:

	  Debug.watchSummary "Number of clicks" length clicks

Again, this evaluates to `clicks` but we get to see how long that list is in
the debugger.
-}
watchSummary : String -> (a -> b) -> a -> a
watchSummary = Native.Debug.watchSummary

{-| Trace all past positions of a `Form` in the debugger. Add this to a `Form`
and you will see a line tracing its entire history.
-}
trace : String -> Form -> Form
trace = Native.Debug.tracePath
