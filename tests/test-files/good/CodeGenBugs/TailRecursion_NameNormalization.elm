-- From https://github.com/elm-lang/elm-compiler/issues/1226
--
-- When code can be tail call optimized, one of the arguments turns into
-- the loop variable. Its name must be properly normalized, for example,
-- to avoid primes.


import Html

go n' = if n' > 0 then go (n'-1) else 1

main = Html.text (toString (go 5))
