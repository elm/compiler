
module Bits where

{-| Library for [bitwise operations](http://en.wikipedia.org/wiki/Bitwise_operation).

@docs and, or, xor, complement, shiftLeft, shiftRight, shiftRightLogical
-}

import Native.Bits

{-| Performs the AND operation on each pair of bits.
-}
and : Int -> Int -> Int
and = Native.Bits.and

{-| Performs the OR operation on each pair of bits.
-}
or : Int -> Int -> Int
or = Native.Bits.or

{-| Performs the XOR operation on each pair of bits.
-}
xor : Int -> Int -> Int
xor = Native.Bits.xor

{-| Performs the NOT operation on each bit.
-}
complement : Int -> Int
complement = Native.Bits.complement

{-| Shift bits to the left by a given offset, filling new bits with zeros.
-}
shiftLeft : Int -> Int -> Int
shiftLeft = Native.Bits.shiftLeft

{-| Shift bits to the right by a given offset, filling new bits with
whatever is the topmost bit. This is an [arithmatic right
shift](http://en.wikipedia.org/wiki/Bitwise_operation#Arithmetic_shift),
often written (>>), and sometimes called a &ldquo;sign-propagating&rdquo;
right shift because it fills empty spots with copies of the highest bit.
-}
shiftRight : Int -> Int -> Int
shiftRight = Native.Bits.shiftRightArithmatic

{-| Shift bits to the right by a given offset, filling new bits with
zeros. This is an [logical right
shift](http://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift), often written (>>>),
and sometimes called a &ldquo;zero-fill&rdquo; right shift because it fills empty spots
with zeros.
-}
shiftRightLogical : Int -> Int -> Int
shiftRightLogical = Native.Bits.shiftRightLogical

