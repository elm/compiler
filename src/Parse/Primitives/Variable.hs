{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, UnboxedTuples, OverloadedStrings #-}
module Parse.Primitives.Variable
  ( lower
  , upper
  , moduleName
  , foreignUpper
  , foreignAlpha
  , chompInnerChars
  , getInnerWidth
  , getInnerWidthHelp
  )
  where


import Control.Exception (assert)
import Data.Bits ((.&.), (.|.), shiftL)
import qualified Data.ByteString.Internal as B
import qualified Data.Char as Char
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Word (Word8)

import Parse.Primitives.Internals (Parser(..), State(..), expect, noError, unsafeIndex)
import qualified Reporting.Error.Syntax as E



-- LOCAL UPPER


upper :: Parser Text
upper =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
    let (# newOffset, newCol #) = chompUpper fp offset terminal col in
    if offset == newOffset then
      eerr (expect row col ctx E.CapVar)
    else
      let !variable = Text.decodeUtf8 (B.PS fp offset (newOffset - offset)) in
      cok variable (State fp newOffset terminal indent row newCol ctx) noError



-- LOCAL LOWER


lower :: Parser Text
lower =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
    let (# newOffset, newCol #) = chompLower fp offset terminal col in
    if offset == newOffset then
      eerr (expect row col ctx E.LowVar)
    else
      let !variable = Text.decodeUtf8 (B.PS fp offset (newOffset - offset)) in
      if Set.member variable reservedWords then
        eerr (expect row col ctx E.LowVar)
      else
        cok variable (State fp newOffset terminal indent row newCol ctx) noError


{-# NOINLINE reservedWords #-}
reservedWords :: Set.Set Text
reservedWords =
  Set.fromList
    [ "if", "then", "else"
    , "case", "of"
    , "let", "in"
    , "type"
    , "module", "where"
    , "import", "exposing"
    , "as"
    , "port"
    ]



-- MODULE NAME


moduleName :: Parser Text
moduleName =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
    let (# newOffset, newCol #) = chompUpper fp offset terminal col in
    case moduleNameHelp fp newOffset terminal newCol of
      Bad badCol ->
        eerr (expect row badCol ctx E.CapVar)

      Good end endCol ->
        let
          !name = Text.decodeUtf8 (B.PS fp offset (end - offset))
          !newState = State fp end terminal indent row endCol ctx
        in
        cok name newState noError


data ModuleName = Bad Int | Good Int Int


moduleNameHelp :: ForeignPtr Word8 -> Int -> Int -> Int -> ModuleName
moduleNameHelp fp offset terminal col =
  if isDot fp offset terminal then
    let
      !offset1 = offset + 1
      (# newOffset, newCol #) = chompUpper fp offset1 terminal (col + 1)
    in
    if offset1 == newOffset then
      Bad newCol
    else
      moduleNameHelp fp newOffset terminal newCol

  else
    Good offset col



-- FOREIGN UPPER


foreignUpper :: Parser (Maybe Text, Text)
foreignUpper =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
    let (# start, end, newCol #) = foreignUpperHelp fp offset terminal col in
    if start == end then
      eerr (expect row newCol ctx E.CapVar)
    else
      let
        !newState = State fp end terminal indent row newCol ctx
        !variable = Text.decodeUtf8 (B.PS fp start (end - start))
        !foreign =
          if start == offset then
            (Nothing, variable)
          else
            let !home = Text.decodeUtf8 (B.PS fp offset ((start - 1) - offset)) in
            (Just home, variable)
      in
      cok foreign newState noError


foreignUpperHelp :: ForeignPtr Word8 -> Int -> Int -> Int -> (# Int, Int, Int #)
foreignUpperHelp fp offset terminal col =
  let
    (# newOffset, newCol #) = chompUpper fp offset terminal col
  in
  if offset == newOffset then
    (# offset, offset, col #)

  else if isDot fp newOffset terminal then
    foreignUpperHelp fp (newOffset + 1) terminal (newCol + 1)

  else
    (# offset, newOffset, newCol #)



-- FOREIGN ALPHA


foreignAlpha :: Parser (Maybe Text, Text)
foreignAlpha =
  Parser $ \(State fp offset terminal indent row col ctx) cok _ _ eerr ->
    let (# start, end, newCol #) = foreignAlphaHelp fp offset terminal col in
    if start == end then
      eerr (E.ParseError row newCol (E.Theories ctx [E.LowVar, E.CapVar]))
    else
      let
        !newState = State fp end terminal indent row newCol ctx
        !variable = Text.decodeUtf8 (B.PS fp start (end - start))
      in
      if start == offset then
        if Set.member variable reservedWords then
          eerr noError
        else
          cok (Nothing, variable) newState noError
      else
        let !home = Text.decodeUtf8 (B.PS fp offset ((start - 1) - offset)) in
        cok (Just home, variable) newState noError


foreignAlphaHelp :: ForeignPtr Word8 -> Int -> Int -> Int -> (# Int, Int, Int #)
foreignAlphaHelp fp offset terminal col =
  let
    (# lowerOffset, lowerCol #) = chompLower fp offset terminal col
  in
  if offset < lowerOffset then
    (# offset, lowerOffset, lowerCol #)

  else
    let
      (# upperOffset, upperCol #) = chompUpper fp offset terminal col
    in
    if offset == upperOffset then
      (# offset, offset, col #)

    else if isDot fp upperOffset terminal then
      foreignAlphaHelp fp (upperOffset + 1) terminal (upperCol + 1)

    else
      (# offset, upperOffset, upperCol #)



---- CHAR CHOMPERS ----



-- DOTS


{-# INLINE isDot #-}
isDot :: ForeignPtr Word8 -> Int -> Int -> Bool
isDot fp offset terminal =
  offset < terminal && unsafeIndex fp offset == 0x2e {- . -}



-- UPPER CHARS


chompUpper :: ForeignPtr Word8 -> Int -> Int -> Int -> (# Int, Int #)
chompUpper fp offset terminal col =
  let !width = getUpperWidth fp offset terminal in
  if width == 0 then
    (# offset, col #)
  else
    chompInnerChars fp (offset + width) terminal (col + 1)


{-# INLINE getUpperWidth #-}
getUpperWidth :: ForeignPtr Word8 -> Int -> Int -> Int
getUpperWidth fp offset terminal =
  if offset < terminal then
    getUpperWidthHelp fp offset terminal (unsafeIndex fp offset)
  else
    0


{-# INLINE getUpperWidthHelp #-}
getUpperWidthHelp :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Int
getUpperWidthHelp fp offset terminal word
  | 0x41 {- A -} <= word && word <= 0x5A {- Z -} = 1
  | word < 0xc0 = 0
  | word < 0xe0 = if Char.isUpper (getChar2 fp offset terminal word) then 2 else 0
  | word < 0xf0 = if Char.isUpper (getChar3 fp offset terminal word) then 3 else 0
  | word < 0xf8 = if Char.isUpper (getChar4 fp offset terminal word) then 4 else 0
  | True        = 0



-- LOWER CHARS


chompLower :: ForeignPtr Word8 -> Int -> Int -> Int -> (# Int, Int #)
chompLower fp offset terminal col =
  let !width = getLowerWidth fp offset terminal in
  if width == 0 then
    (# offset, col #)
  else
    chompInnerChars fp (offset + width) terminal (col + 1)


{-# INLINE getLowerWidth #-}
getLowerWidth :: ForeignPtr Word8 -> Int -> Int -> Int
getLowerWidth fp offset terminal =
  if offset < terminal then
    getLowerWidthHelp fp offset terminal (unsafeIndex fp offset)
  else
    0


{-# INLINE getLowerWidthHelp #-}
getLowerWidthHelp :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Int
getLowerWidthHelp fp offset terminal word
  | 0x61 {- a -} <= word && word <= 0x7A {- z -} = 1
  | word < 0xc0 = 0
  | word < 0xe0 = if Char.isLower (getChar2 fp offset terminal word) then 2 else 0
  | word < 0xf0 = if Char.isLower (getChar3 fp offset terminal word) then 3 else 0
  | word < 0xf8 = if Char.isLower (getChar4 fp offset terminal word) then 4 else 0
  | True        = 0



-- INNER CHARS


chompInnerChars :: ForeignPtr Word8 -> Int -> Int -> Int -> (# Int, Int #)
chompInnerChars fp !offset terminal !col =
  let !width = getInnerWidth fp offset terminal in
  if width == 0 then
    (# offset, col #)
  else
    chompInnerChars fp (offset + width) terminal (col + 1)


getInnerWidth :: ForeignPtr Word8 -> Int -> Int -> Int
getInnerWidth fp offset terminal =
  if offset < terminal then
    getInnerWidthHelp fp offset terminal (unsafeIndex fp offset)
  else
    0


{-# INLINE getInnerWidthHelp #-}
getInnerWidthHelp :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Int
getInnerWidthHelp fp offset terminal word
  | 0x61 {- a -} <= word && word <= 0x7A {- z -} = 1
  | 0x41 {- A -} <= word && word <= 0x5A {- Z -} = 1
  | 0x30 {- 0 -} <= word && word <= 0x39 {- 9 -} = 1
  | word == 0x5F {- _ -} = 1
  | word < 0xc0 = 0
  | word < 0xe0 = if Char.isAlpha (getChar2 fp offset terminal word) then 2 else 0
  | word < 0xf0 = if Char.isAlpha (getChar3 fp offset terminal word) then 3 else 0
  | word < 0xf8 = if Char.isAlpha (getChar4 fp offset terminal word) then 4 else 0
  | True        = 0



-- EXTRACT CHARACTERS


push :: Word8 -> Int -> Int
push word code =
  assert (word .&. 0xc0 == 0x80) (
    shiftL code 6 .|. fromEnum (word .&. 0x3f)
  )


getChar2 :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Char
getChar2 fp offset terminal word =
  assert (offset + 2 <= terminal) (
    let
      !word1 = word .&. 0x1f
      !word2 = unsafeIndex fp (offset + 1)
      !code = push word2 (fromEnum word1)
    in
    assert (0x80 <= code) (
      toEnum code
    )
  )


getChar3 :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Char
getChar3 fp offset terminal word =
  assert (offset + 3 <= terminal) (
    let
      !word1 = word .&. 0x0f
      !word2 = unsafeIndex fp (offset + 1)
      !word3 = unsafeIndex fp (offset + 2)
      !code = push word3 (push word2 (fromEnum word1))
    in
    assert ((0x800 <= code && code < 0xd800) || (0xdfff < code && code < 0xfffe)) (
      toEnum code
    )
  )


getChar4 :: ForeignPtr Word8 -> Int -> Int -> Word8 -> Char
getChar4 fp offset terminal word =
  assert (offset + 4 <= terminal) (
    let
      !word1 = word .&. 0x07
      !word2 = unsafeIndex fp (offset + 1)
      !word3 = unsafeIndex fp (offset + 2)
      !word4 = unsafeIndex fp (offset + 3)
      !code = push word4 (push word3 (push word2 (fromEnum word1)))
    in
    assert (0x10000 <= code && code < 0x110000) (
      toEnum code
    )
  )
