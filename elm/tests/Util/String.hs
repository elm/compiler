module Util.String where

import qualified Data.Text as T

replace :: String -> String -> String -> String
replace a b c = T.unpack (T.replace (T.pack a) (T.pack b) (T.pack c))

escape :: String -> String
escape =
  (replace "\n" "\\n") .
  (replace "\t" "\\t") .
  (replace "\"" "\\\"") .
  (replace "\\" "\\\\")

-- (a -> t) -> a -> b -> c -> (t -> t -> t -> z) -> z
