module Prelude where
{-| Everything that is automatically imported -}

import Native.Show

-- Convert almost any value to its string representation.
show : a -> String
show = Native.Show.show