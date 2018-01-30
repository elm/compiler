{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, UnboxedTuples, OverloadedStrings #-}
module Parse.Primitives.Kernel
  ( Special(..)
  , chunk
  )
  where


import Prelude hiding (length)
import qualified Data.ByteString.Internal as B
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)

import qualified Elm.Name as N
import Parse.Primitives.Internals (Parser(..), State(..), noError)
import qualified Parse.Primitives.Internals as I
import qualified Parse.Primitives.Variable as Var



-- SPECIAL


data Special
  = Enum Word8 N.Name
  | Prod
  | Debug
  | Import N.Name
  | JsField N.Name
  | ElmField N.Name



-- CHUNK


chunk :: Parser (B.ByteString, Maybe Special)
chunk =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ _ ->
    let
      (# maybeSpecial, jsOffset, newOffset, newRow, newCol #) =
        chompChunk fp offset terminal row col

      !javascript = B.PS fp offset (jsOffset - offset)
      !newState = State fp newOffset terminal indent newRow newCol ctx
    in
      cok (javascript, maybeSpecial) newState noError



-- CHOMP CHUNK


chompChunk :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> (# Maybe Special, Int, Int, Int, Int #)
chompChunk fp offset terminal row col =
  if offset >= terminal then
    (# Nothing, offset, offset, row, col #)

  else
    let !word = I.unsafeIndex fp offset in
    if word == 0x5F {- _ -} then

      let
        !offset1 = offset + 1
        !offset3 = offset + 3
      in
      if offset3 <= terminal && I.unsafeIndex fp offset1 == 0x5F {- _ -} then
        chompSpecial fp offset3 terminal row (col + 3) offset
      else
        chompChunk fp offset1 terminal row (col + 1)

    else if word == 0x0A {- \n -} then
      chompChunk fp (offset + 1) terminal (row + 1) 1

    else
      let !newOffset = offset + I.getCharWidth fp offset terminal word in
      chompChunk fp newOffset terminal row (col + 1)



-- CHOMP TAG


-- relies on external checks in chompChunk
chompSpecial :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Int -> (# Maybe Special, Int, Int, Int, Int #)
chompSpecial fp offset terminal row col jsOffset =
  let
    (# newOffset, newCol #) =
      Var.chompInnerChars fp offset terminal col

    !tagOffset = offset - 1
    !word = I.unsafeIndex fp tagOffset

    !special =
      if word == 0x24 {- $ -} then
        ElmField (N.fromForeignPtr fp offset (newOffset - offset))

      else
        let !name = N.fromForeignPtr fp tagOffset (newOffset - tagOffset) in
        if 0x30 <= word && word <= 0x39 then
          Enum (fromIntegral (word - 0x30)) name

        else if 0x61 {- a -} <= word && word <= 0x7A {- z -} then
          JsField name

        else if name == "DEBUG" then
          Debug

        else if name == "PROD" then
          Prod

        else
          Import name
  in
    (# Just special, jsOffset, newOffset, row, newCol #)
