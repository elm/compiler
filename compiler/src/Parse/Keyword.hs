{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module Parse.Keyword
  ( type_, alias_, port_
  , if_, then_, else_
  , case_, of_
  , let_, in_
  , infix_, left_, right_, non_
  , module_, import_, exposing_, as_
  , effect_, where_, command_, subscription_
  , k4, k5
  )
  where


import Foreign.Ptr (plusPtr)
import Data.Word (Word8)

import Parse.Primitives (Parser, Row, Col)
import qualified Parse.Variable as Var
import qualified Parse.Primitives as P



-- DECLARATIONS


type_ :: (Row -> Col -> x) -> Parser x ()
type_ tx = k4 0x74 0x79 0x70 0x65 tx

alias_ :: (Row -> Col -> x) -> Parser x ()
alias_ tx = k5 0x61 0x6C 0x69 0x61 0x73 tx

port_ :: (Row -> Col -> x) -> Parser x ()
port_ tx = k4 0x70 0x6F 0x72 0x74 tx



-- IF EXPRESSIONS


if_ :: (Row -> Col -> x) -> Parser x ()
if_ tx = k2 0x69 0x66 tx

then_ :: (Row -> Col -> x) -> Parser x ()
then_ tx = k4 0x74 0x68 0x65 0x6E tx

else_ :: (Row -> Col -> x) -> Parser x ()
else_ tx = k4 0x65 0x6C 0x73 0x65 tx



-- CASE EXPRESSIONS


case_ :: (Row -> Col -> x) -> Parser x ()
case_ tx = k4 0x63 0x61 0x73 0x65 tx

of_ :: (Row -> Col -> x) -> Parser x ()
of_ tx = k2 0x6F 0x66 tx



-- LET EXPRESSIONS


let_ :: (Row -> Col -> x) -> Parser x ()
let_ tx = k3 0x6C 0x65 0x74 tx

in_ :: (Row -> Col -> x) -> Parser x ()
in_ tx = k2 0x69 0x6E tx



-- INFIXES


infix_ :: (Row -> Col -> x) -> Parser x ()
infix_ tx = k5 0x69 0x6E 0x66 0x69 0x78 tx

left_ :: (Row -> Col -> x) -> Parser x ()
left_ tx = k4 0x6C 0x65 0x66 0x74 tx

right_ :: (Row -> Col -> x) -> Parser x ()
right_ tx = k5 0x72 0x69 0x67 0x68 0x74 tx

non_ :: (Row -> Col -> x) -> Parser x ()
non_ tx = k3 0x6E 0x6F 0x6E tx



-- IMPORTS


module_ :: (Row -> Col -> x) -> Parser x ()
module_ tx = k6 0x6D 0x6F 0x64 0x75 0x6C 0x65 tx

import_ :: (Row -> Col -> x) -> Parser x ()
import_ tx = k6 0x69 0x6D 0x70 0x6F 0x72 0x74 tx

exposing_ :: (Row -> Col -> x) -> Parser x ()
exposing_ tx = k8 0x65 0x78 0x70 0x6F 0x73 0x69 0x6E 0x67 tx

as_ :: (Row -> Col -> x) -> Parser x ()
as_ tx = k2 0x61 0x73 tx



-- EFFECTS


effect_ :: (Row -> Col -> x) -> Parser x ()
effect_ tx = k6 0x65 0x66 0x66 0x65 0x63 0x74 tx

where_ :: (Row -> Col -> x) -> Parser x ()
where_ tx = k5 0x77 0x68 0x65 0x72 0x65 tx

command_ :: (Row -> Col -> x) -> Parser x ()
command_ tx = k7 0x63 0x6F 0x6D 0x6D 0x61 0x6E 0x64 tx

subscription_ :: (Row -> Col -> x) -> Parser x ()
subscription_ toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    let !pos12 = plusPtr pos 12 in
    if pos12 <= end
      && P.unsafeIndex (        pos   ) == 0x73
      && P.unsafeIndex (plusPtr pos  1) == 0x75
      && P.unsafeIndex (plusPtr pos  2) == 0x62
      && P.unsafeIndex (plusPtr pos  3) == 0x73
      && P.unsafeIndex (plusPtr pos  4) == 0x63
      && P.unsafeIndex (plusPtr pos  5) == 0x72
      && P.unsafeIndex (plusPtr pos  6) == 0x69
      && P.unsafeIndex (plusPtr pos  7) == 0x70
      && P.unsafeIndex (plusPtr pos  8) == 0x74
      && P.unsafeIndex (plusPtr pos  9) == 0x69
      && P.unsafeIndex (plusPtr pos 10) == 0x6F
      && P.unsafeIndex (plusPtr pos 11) == 0x6E
      && Var.getInnerWidth pos12 end == 0
    then
      let !s = P.State src pos12 end indent row (col + 12) in cok () s
    else
      eerr row col toError



-- KEYWORDS


k2 :: Word8 -> Word8 -> (Row -> Col -> x) -> Parser x ()
k2 w1 w2 toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    let !pos2 = plusPtr pos 2 in
    if pos2 <= end
      && P.unsafeIndex (        pos  ) == w1
      && P.unsafeIndex (plusPtr pos 1) == w2
      && Var.getInnerWidth pos2 end == 0
    then
      let !s = P.State src pos2 end indent row (col + 2) in cok () s
    else
      eerr row col toError


k3 :: Word8 -> Word8 -> Word8 -> (Row -> Col -> x) -> Parser x ()
k3 w1 w2 w3 toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    let !pos3 = plusPtr pos 3 in
    if pos3 <= end
      && P.unsafeIndex (        pos  ) == w1
      && P.unsafeIndex (plusPtr pos 1) == w2
      && P.unsafeIndex (plusPtr pos 2) == w3
      && Var.getInnerWidth pos3 end == 0
    then
      let !s = P.State src pos3 end indent row (col + 3) in cok () s
    else
      eerr row col toError


k4 :: Word8 -> Word8 -> Word8 -> Word8 -> (Row -> Col -> x) -> Parser x ()
k4 w1 w2 w3 w4 toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    let !pos4 = plusPtr pos 4 in
    if pos4 <= end
      && P.unsafeIndex (        pos  ) == w1
      && P.unsafeIndex (plusPtr pos 1) == w2
      && P.unsafeIndex (plusPtr pos 2) == w3
      && P.unsafeIndex (plusPtr pos 3) == w4
      && Var.getInnerWidth pos4 end == 0
    then
      let !s = P.State src pos4 end indent row (col + 4) in cok () s
    else
      eerr row col toError


k5 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Row -> Col -> x) -> Parser x ()
k5 w1 w2 w3 w4 w5 toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    let !pos5 = plusPtr pos 5 in
    if pos5 <= end
      && P.unsafeIndex (        pos  ) == w1
      && P.unsafeIndex (plusPtr pos 1) == w2
      && P.unsafeIndex (plusPtr pos 2) == w3
      && P.unsafeIndex (plusPtr pos 3) == w4
      && P.unsafeIndex (plusPtr pos 4) == w5
      && Var.getInnerWidth pos5 end == 0
    then
      let !s = P.State src pos5 end indent row (col + 5) in cok () s
    else
      eerr row col toError


k6 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Row -> Col -> x) -> Parser x ()
k6 w1 w2 w3 w4 w5 w6 toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    let !pos6 = plusPtr pos 6 in
    if pos6 <= end
      && P.unsafeIndex (        pos  ) == w1
      && P.unsafeIndex (plusPtr pos 1) == w2
      && P.unsafeIndex (plusPtr pos 2) == w3
      && P.unsafeIndex (plusPtr pos 3) == w4
      && P.unsafeIndex (plusPtr pos 4) == w5
      && P.unsafeIndex (plusPtr pos 5) == w6
      && Var.getInnerWidth pos6 end == 0
    then
      let !s = P.State src pos6 end indent row (col + 6) in cok () s
    else
      eerr row col toError


k7 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Row -> Col -> x) -> Parser x ()
k7 w1 w2 w3 w4 w5 w6 w7 toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    let !pos7 = plusPtr pos 7 in
    if pos7 <= end
      && P.unsafeIndex (        pos  ) == w1
      && P.unsafeIndex (plusPtr pos 1) == w2
      && P.unsafeIndex (plusPtr pos 2) == w3
      && P.unsafeIndex (plusPtr pos 3) == w4
      && P.unsafeIndex (plusPtr pos 4) == w5
      && P.unsafeIndex (plusPtr pos 5) == w6
      && P.unsafeIndex (plusPtr pos 6) == w7
      && Var.getInnerWidth pos7 end == 0
    then
      let !s = P.State src pos7 end indent row (col + 7) in cok () s
    else
      eerr row col toError


k8 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Row -> Col -> x) -> Parser x ()
k8 w1 w2 w3 w4 w5 w6 w7 w8 toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    let !pos8 = plusPtr pos 8 in
    if pos8 <= end
      && P.unsafeIndex (        pos  ) == w1
      && P.unsafeIndex (plusPtr pos 1) == w2
      && P.unsafeIndex (plusPtr pos 2) == w3
      && P.unsafeIndex (plusPtr pos 3) == w4
      && P.unsafeIndex (plusPtr pos 4) == w5
      && P.unsafeIndex (plusPtr pos 5) == w6
      && P.unsafeIndex (plusPtr pos 6) == w7
      && P.unsafeIndex (plusPtr pos 7) == w8
      && Var.getInnerWidth pos8 end == 0
    then
      let !s = P.State src pos8 end indent row (col + 8) in cok () s
    else
      eerr row col toError
