module Debug.Logging where
{-|

#Logging
@docs unsafePrintLogMessage, unsafeShowValueInLog, unsafeShowLabledValueInLog

-}

import Prelude
import open List
import Native.Debug.Logging

{-| Print the value as a string to the error console.

Example:

    e : Int
    e = Debug.Logging.unsafeShowValueInLog 4

Prints:

    Error: 4

Note: Does not work for values of type Signal
-}
unsafeShowValueInLog : a -> a
unsafeShowValueInLog a = (\message -> a )(Native.Debug.Logging.unsafePrintLogMessage  (Prelude.show a))

{-|
Print a string to the error console.

The seccond value is passed through identity.  This allows you to embed this function in pure code.

Example:

    v : Int
    v = Debug.Logging.unsafePrintLogMessage "Hey!" 1

Prints:

    Error: Hey!
-}
unsafePrintLogMessage : String -> a -> a
unsafePrintLogMessage message a =
 (\_ -> a) (Native.Debug.Logging.unsafePrintLogMessage message)

{-|
Prints a value to the error console prepended by the given lable.

Example:

    b : Int
    b = unsafeShowLabledValueInLog "b:" 2

Prints:

    Error: b:2

Note: Does not work for values of type Signal
-}
unsafeShowLabledValueInLog : String -> a -> a
unsafeShowLabledValueInLog label a =
 (\_ -> a) (Native.Debug.Logging.unsafePrintLogMessage (label ++ (Prelude.show a)))