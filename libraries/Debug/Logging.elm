module Debug.Logging where
{-|

#Logging
@docs unsafePrintLogMessage

-}

import Prelude
import Native.Debug.Logging

{-| Print the value as a string to the error console. -}
unsafePrintLogMessage : a -> a
unsafePrintLogMessage a = (\message -> a )(Native.Debug.Logging.unsafePrintLogMessage  (Prelude.show a))