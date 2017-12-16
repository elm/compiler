{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, UnboxedTuples, OverloadedStrings #-}
module Parse.Primitives.Whitespace
  ( SPos(..)
  , whitespace
  , docComment
  , chompUntilDocs
  )
  where


import qualified Data.ByteString.Internal as B
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Word (Word8)

import Parse.Primitives.Internals (Parser(..), State(..), noError)
import qualified Parse.Primitives.Internals as I
import qualified Parse.Primitives.Symbol as Symbol
import qualified Reporting.Error.Syntax as E
import qualified Reporting.Region as R



-- WHITESPACE


newtype SPos = SPos R.Position


whitespace :: Parser SPos
whitespace =
  Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ _ ->
    case eatSpaces fp offset terminal row col of
      Err err ->
        cerr err

      Ok newOffset newRow newCol ->
        let
          !spos = SPos (R.Position newRow newCol)
          !newState = State fp newOffset terminal indent newRow newCol ctx
        in
        cok spos newState noError


data Result
  = Err E.ParseError
  | Ok Int Int Int



-- EAT SPACES


eatSpaces :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Result
eatSpaces fp offset terminal row col =
  if offset >= terminal then
    Ok offset row col

  else
    case I.unsafeIndex fp offset of
      0x20 {-   -} ->
        eatSpaces fp (offset + 1) terminal row (col + 1)

      0x0A {- \n -} ->
        eatSpaces fp (offset + 1) terminal (row + 1) 1

      0x7B {- { -} ->
        eatMultiComment fp offset terminal row col

      0x2D {- - -} ->
        let !offset1 = offset + 1 in
        if offset1 < terminal && I.unsafeIndex fp offset1 == 0x2D {- - -} then
          eatLineComment fp (offset + 2) terminal row (col + 2)
        else
          Ok offset row col

      0x0D {- \r -} ->
        eatSpaces fp (offset + 1) terminal row col

      0x09 {- \t -} ->
        Err (E.ParseError row col E.Tab)

      _ ->
        Ok offset row col



-- LINE COMMENTS


eatLineComment :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Result
eatLineComment fp offset terminal row col =
  if offset >= terminal then
    Ok offset row col

  else
    let !word = I.unsafeIndex fp offset in
    if word == 0x0A {- \n -} then
      eatSpaces fp (offset + 1) terminal (row + 1) 1
    else
      let !newOffset = offset + I.getCharWidth fp offset terminal word in
      eatLineComment fp newOffset terminal row (col + 1)



-- MULTI COMMENTS


eatMultiComment :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Result
eatMultiComment fp offset terminal row col =
  let
    !offset1 = offset + 1
    !offset2 = offset + 2
  in
  if offset2 >= terminal then
    Ok offset row col

  else
    let
      !yesDash = I.unsafeIndex fp offset1 == 0x2D {- - -}
      !noBar   = I.unsafeIndex fp offset2 /= 0x7C {- | -}
    in
      if yesDash && noBar then
        case eatMultiCommentHelp fp offset2 terminal row (col + 2) 1 of
          Ok newOffset newRow newCol ->
            eatSpaces fp newOffset terminal newRow newCol

          err@(Err _) ->
            err

      else
        Ok offset row col


eatMultiCommentHelp :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> Int -> Result
eatMultiCommentHelp fp offset terminal row col openComments =
  if offset >= terminal then
    Err (E.ParseError row col E.EndOfFile_Comment)

  else
    let !word = I.unsafeIndex fp offset in
    if word == 0x0A {- \n -} then
      eatMultiCommentHelp fp (offset + 1) terminal (row + 1) 1 openComments

    else if word == 0x2D {- - -} && I.isWord fp (offset + 1) terminal 0x7D {- } -} then
      if openComments == 1 then
        Ok (offset + 2) row (col + 2)
      else
        eatMultiCommentHelp fp (offset + 2) terminal row (col + 2) (openComments - 1)

    else if word == 0x7B {- { -} && I.isWord fp (offset + 1) terminal 0x2D {- - -} then
      eatMultiCommentHelp fp (offset + 2) terminal row (col + 2) (openComments + 1)

    else
      let !newOffset = offset + I.getCharWidth fp offset terminal word in
      eatMultiCommentHelp fp newOffset terminal row (col + 1) openComments



-- DOCUMENTATION COMMENT


docComment :: Parser B.ByteString
docComment =
  do  Symbol.elmDocCommentOpen
      Parser $ \(State fp offset terminal indent row col ctx) cok cerr _ _ ->
        case eatMultiCommentHelp fp offset terminal row col 1 of
          Err err ->
            cerr err

          Ok newOffset newRow newCol ->
            let
              !comment = B.PS fp offset (newOffset - offset - 2)
              !newState = State fp newOffset terminal indent newRow newCol ctx
            in
            cok comment newState noError



-- PARSING DOCS


chompUntilDocs :: Parser Bool
chompUntilDocs =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ _ ->
    let
      (# isStart, newOffset, newRow, newCol #) =
        eatDocs fp offset terminal row col

      !newState =
        State fp newOffset terminal indent newRow newCol ctx
    in
      cok isStart newState noError


eatDocs :: ForeignPtr Word8 -> Int -> Int -> Int -> Int -> (# Bool, Int, Int, Int #)
eatDocs fp offset terminal row col =
  if offset >= terminal then
    (# False, offset, row, col #)

  else if isDocsStart fp offset terminal then
    (# True, offset + 5, row, col + 5 #)

  else
    let !word = I.unsafeIndex fp offset in
    if word == 0x0A {- \n -} then
      eatDocs fp (offset + 1) terminal (row + 1) 1

    else
      let !newOffset = offset + I.getCharWidth fp offset terminal word in
      eatDocs fp newOffset terminal row (col + 1)


isDocsStart :: ForeignPtr Word8 -> Int -> Int -> Bool
isDocsStart =
  let (B.PS dfp doff dlen) = docsStart in
  I.isSubstring dfp doff dlen


{-# NOINLINE docsStart #-}
docsStart :: B.ByteString
docsStart =
  "@docs"
