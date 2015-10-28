
-- Issue: https://github.com/elm-lang/elm-compiler/issues/1115
-- Nested patterns are not correctly handled and fail with unreachable assertion error.

foo (({f1} as r1) as r2) = r1