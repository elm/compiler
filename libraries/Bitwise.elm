module Bitwise where

{-| Library for [bitwise operations](http://en.wikipedia.org/wiki/Bitwise_operation).

# Basic Operations

@docs and, or, xor, complement

# Bit Shifts

@docs shiftLeft, shiftRight, shiftRightLogical
-}

import Native.Bitwise

{-| Bitwise AND
-}
and : Int -> Int -> Int
and = Native.Bitwise.and

{-| Bitwise OR
-}
or : Int -> Int -> Int
or = Native.Bitwise.or

{-| Bitwise XOR
-}
xor : Int -> Int -> Int
xor = Native.Bitwise.xor

{-| Flip each bit individually, often called bitwise NOT
-}
complement : Int -> Int
complement = Native.Bitwise.complement

{-| Shift bits to the left by a given offset, filling new bits with zeros.
This can be used to multiply numbers by powers of two.

    8 `shiftLeft` 1 == 16
    8 `shiftLeft` 2 == 32
-}
shiftLeft : Int -> Int -> Int
shiftLeft = Native.Bitwise.shiftLeft

{-| Shift bits to the right by a given offset, filling new bits with
whatever is the topmost bit. This can be used to divide numbers by powers of two.

     32 `shiftRight` 1 == 16
     32 `shiftRight` 2 == 8
    -32 `shiftRight` 1 == -16

This is called an [arithmatic right
shift](http://en.wikipedia.org/wiki/Bitwise_operation#Arithmetic_shift),
often written (>>), and sometimes called a sign-propagating
right shift because it fills empty spots with copies of the highest bit.
-}
shiftRight : Int -> Int -> Int
shiftRight = Native.Bitwise.shiftRightArithmatic

{-| Shift bits to the right by a given offset, filling new bits with
zeros.

     32 `shiftRightLogical` 1 == 16
     32 `shiftRightLogical` 2 == 8
    -32 `shiftRightLogical` 1 == 2147483632

This is called an [logical right
shift](http://en.wikipedia.org/wiki/Bitwise_operation#Logical_shift), often written (>>>),
and sometimes called a zero-fill right shift because it fills empty spots
with zeros.
-}
shiftRightLogical : Int -> Int -> Int
shiftRightLogical = Native.Bitwise.shiftRightLogical

