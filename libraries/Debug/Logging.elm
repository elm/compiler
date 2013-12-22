module Debug.Logging where
{-|

#Logging
@docs unsafePrintLogMessage

-}

import Prelude
import Native.Debug.Logging

{-| Print the value as a string to the error console.

Note: Does not work for values of type Signal
-}
unsafePrintLogMessage : a -> a
unsafePrintLogMessage a = (\message -> a )(Native.Debug.Logging.unsafePrintLogMessage  (Prelude.show a))
