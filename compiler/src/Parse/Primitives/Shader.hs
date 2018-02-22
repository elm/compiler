{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module Parse.Primitives.Shader
  ( block
  , failure
  )
  where


import qualified Data.ByteString.Internal as B
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Word (Word8)

import Parse.Primitives.Internals (Parser(..), State(..), noError)
import qualified Parse.Primitives.Internals as I
import qualified Parse.Primitives.Symbol as Symbol
import qualified Reporting.Error.Syntax as E



-- SHADER


failure :: Int -> Int -> Text.Text -> Parser a
failure row col msg =
  Parser $ \_ _ cerr _ _ ->
    cerr (E.ParseError row col (E.BadShader msg))


block :: Parser Text.Text
block =
  do  Symbol.shaderBlockOpen
      Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ _ ->
        case eatShader fp offset terminal row col of
          Err ->
            cerr (E.ParseError row col E.EndOfFile_Shader)

          Ok newOffset newRow newCol ->
            let
              !size = newOffset - offset
              !shader = Text.decodeUtf8 (B.PS fp offset size)
              !newState = State fp (newOffset + 2) terminal indent newRow newCol ctx
            in
              cok shader newState noError


data Result
  = Err
  | Ok Int Int Int


eatShader :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Result
eatShader fp offset terminal row col =
  if offset >= terminal then
    Err

  else
    let !word = I.unsafeIndex fp offset in
    if word == 0x007C {- | -} && I.isWord fp (offset + 1) terminal 0x5D {- ] -} then
      Ok offset row (col + 2)

    else if word == 0x0A {- \n -} then
      eatShader fp (offset + 1) terminal (row + 1) 1

    else
      let !newOffset = offset + I.getCharWidth fp offset terminal word in
      eatShader fp newOffset terminal row (col + 1)
