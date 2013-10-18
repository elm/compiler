module Prelude where
{-| Everything that is automatically imported -}

import Basics (String)
import Maybe (Maybe)
import Native.Prelude
import Native.Show

-- Convert almost any value to its string representation.
show : a -> String
show = Native.Show.show

-- Read an integer from a string
readInt : String -> Maybe Int
readInt = Native.Prelude.readInt

-- Read a float from a string.
readFloat : String -> Maybe Float
readFloat = Native.Prelude.readFloat
