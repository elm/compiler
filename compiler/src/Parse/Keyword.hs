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

import Parse.Utils (Parser)
import qualified Parse.Variable as Var
import qualified Parse.Primitives as P
import qualified Reporting.Error.Syntax as E



-- DECLARATIONS


type_ :: Parser ()
type_ = k4 0x74 0x79 0x70 0x65 E.KeywordType

alias_ :: Parser ()
alias_ = k5 0x61 0x6C 0x69 0x61 0x73 E.KeywordAlias

port_ :: Parser ()
port_ = k4 0x70 0x6F 0x72 0x74 E.KeywordPort



-- IF EXPRESSIONS


if_ :: Parser ()
if_ = k2 0x69 0x66 E.KeywordIf

then_ :: Parser ()
then_ = k4 0x74 0x68 0x65 0x6E E.KeywordThen

else_ :: Parser ()
else_ = k4 0x65 0x6C 0x73 0x65 E.KeywordElse



-- CASE EXPRESSIONS


case_ :: Parser ()
case_ = k4 0x63 0x61 0x73 0x65 E.KeywordCase

of_ :: Parser ()
of_ = k2 0x6F 0x66 E.KeywordOf



-- LET EXPRESSIONS


let_ :: Parser ()
let_ = k3 0x6C 0x65 0x74 E.KeywordLet

in_ :: Parser ()
in_ = k2 0x69 0x6E E.KeywordIn



-- INFIXES


infix_ :: Parser ()
infix_ = k5 0x69 0x6E 0x66 0x69 0x78 E.KeywordInfix

left_ :: Parser ()
left_ = k4 0x6C 0x65 0x66 0x74 E.Associativity

right_ :: Parser ()
right_ = k5 0x72 0x69 0x67 0x68 0x74 E.Associativity

non_ :: Parser ()
non_ = k3 0x6E 0x6F 0x6E E.Associativity



-- IMPORTS


module_ :: Parser ()
module_ = k6 0x6D 0x6F 0x64 0x75 0x6C 0x65 E.KeywordModule

import_ :: Parser ()
import_ = k6 0x69 0x6D 0x70 0x6F 0x72 0x74 E.KeywordImport

exposing_ :: Parser ()
exposing_ = k8 0x65 0x78 0x70 0x6F 0x73 0x69 0x6E 0x67 E.KeywordExposing

as_ :: Parser ()
as_ = k2 0x61 0x73 E.KeywordAs



-- EFFECTS


effect_ :: Parser ()
effect_ = k6 0x65 0x66 0x66 0x65 0x63 0x74 E.EffectStuff

where_ :: Parser ()
where_ = k5 0x77 0x68 0x65 0x72 0x65 E.EffectStuff

command_ :: Parser ()
command_ = k7 0x63 0x6F 0x6D 0x6D 0x61 0x6E 0x64 E.EffectStuff

subscription_ :: Parser ()
subscription_ =
  P.Parser $ \(P.State pos end indent row col ctx) cok _ _ eerr ->
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
      let !s = P.State pos12 end indent row (col + 12) ctx in cok () s
    else
      eerr row col ctx E.EffectStuff



-- KEYWORDS


k2 :: Word8 -> Word8 -> x -> P.Parser c x ()
k2 w1 w2 x =
  P.Parser $ \(P.State pos end indent row col ctx) cok _ _ eerr ->
    let !pos2 = plusPtr pos 2 in
    if pos2 <= end
      && P.unsafeIndex (        pos  ) == w1
      && P.unsafeIndex (plusPtr pos 1) == w2
      && Var.getInnerWidth pos2 end == 0
    then
      let !s = P.State pos2 end indent row (col + 2) ctx in cok () s
    else
      eerr row col ctx x


k3 :: Word8 -> Word8 -> Word8 -> x -> P.Parser c x ()
k3 w1 w2 w3 x =
  P.Parser $ \(P.State pos end indent row col ctx) cok _ _ eerr ->
    let !pos3 = plusPtr pos 3 in
    if pos3 <= end
      && P.unsafeIndex (        pos  ) == w1
      && P.unsafeIndex (plusPtr pos 1) == w2
      && P.unsafeIndex (plusPtr pos 2) == w3
      && Var.getInnerWidth pos3 end == 0
    then
      let !s = P.State pos3 end indent row (col + 3) ctx in cok () s
    else
      eerr row col ctx x


k4 :: Word8 -> Word8 -> Word8 -> Word8 -> x -> P.Parser c x ()
k4 w1 w2 w3 w4 x =
  P.Parser $ \(P.State pos end indent row col ctx) cok _ _ eerr ->
    let !pos4 = plusPtr pos 4 in
    if pos4 <= end
      && P.unsafeIndex (        pos  ) == w1
      && P.unsafeIndex (plusPtr pos 1) == w2
      && P.unsafeIndex (plusPtr pos 2) == w3
      && P.unsafeIndex (plusPtr pos 3) == w4
      && Var.getInnerWidth pos4 end == 0
    then
      let !s = P.State pos4 end indent row (col + 4) ctx in cok () s
    else
      eerr row col ctx x


k5 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> x -> P.Parser c x ()
k5 w1 w2 w3 w4 w5 x =
  P.Parser $ \(P.State pos end indent row col ctx) cok _ _ eerr ->
    let !pos5 = plusPtr pos 5 in
    if pos5 <= end
      && P.unsafeIndex (        pos  ) == w1
      && P.unsafeIndex (plusPtr pos 1) == w2
      && P.unsafeIndex (plusPtr pos 2) == w3
      && P.unsafeIndex (plusPtr pos 3) == w4
      && P.unsafeIndex (plusPtr pos 4) == w5
      && Var.getInnerWidth pos5 end == 0
    then
      let !s = P.State pos5 end indent row (col + 5) ctx in cok () s
    else
      eerr row col ctx x


k6 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> x -> P.Parser c x ()
k6 w1 w2 w3 w4 w5 w6 x =
  P.Parser $ \(P.State pos end indent row col ctx) cok _ _ eerr ->
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
      let !s = P.State pos6 end indent row (col + 6) ctx in cok () s
    else
      eerr row col ctx x


k7 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> x -> P.Parser c x ()
k7 w1 w2 w3 w4 w5 w6 w7 x =
  P.Parser $ \(P.State pos end indent row col ctx) cok _ _ eerr ->
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
      let !s = P.State pos7 end indent row (col + 7) ctx in cok () s
    else
      eerr row col ctx x


k8 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> x -> P.Parser c x ()
k8 w1 w2 w3 w4 w5 w6 w7 w8 x =
  P.Parser $ \(P.State pos end indent row col ctx) cok _ _ eerr ->
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
      let !s = P.State pos8 end indent row (col + 8) ctx in cok () s
    else
      eerr row col ctx x
