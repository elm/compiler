{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, MagicHash, OverloadedStrings, UnboxedTuples #-}
module Parse.Variable
  ( lower
  , upper
  , moduleName
  , Upper(..)
  , foreignUpper
  , foreignAlpha
  , chompInnerChars
  , getUpperWidth
  , getInnerWidth
  , getInnerWidthHelp
  , reservedWords
  )
  where


import qualified Data.Char as Char
import qualified Data.Name as Name
import qualified Data.Set as Set
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)
import GHC.Exts (Char(C#), Int#, (+#), (-#), chr#, uncheckedIShiftL#, word2Int#, word8ToWord#)
import GHC.Word (Word8(W8#))

import qualified AST.Source as Src
import Parse.Primitives (Parser, Row, Col, unsafeIndex)
import qualified Parse.Primitives as P



-- LOCAL UPPER


upper :: (Row -> Col -> x) -> Parser x Name.Name
upper toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    let (# newPos, newCol #) = chompUpper pos end col in
    if pos == newPos then
      eerr row col toError
    else
      let !name = Name.fromPtr pos newPos in
      cok name (P.State src newPos end indent row newCol)



-- LOCAL LOWER


lower :: (Row -> Col -> x) -> Parser x Name.Name
lower toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    let (# newPos, newCol #) = chompLower pos end col in
    if pos == newPos then
      eerr row col toError
    else
      let !name = Name.fromPtr pos newPos in
      if Set.member name reservedWords then
        eerr row col toError
      else
        let
          !newState =
            P.State src newPos end indent row newCol
        in
        cok name newState


{-# NOINLINE reservedWords #-}
reservedWords :: Set.Set Name.Name  -- PERF try using a trie instead
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


moduleName :: (Row -> Col -> x) -> Parser x Name.Name
moduleName toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr eerr ->
    let
      (# pos1, col1 #) = chompUpper pos end col
    in
    if pos == pos1 then
      eerr row col toError
    else
      let
        (# status, newPos, newCol #) = moduleNameHelp pos1 end col1
      in
      case status of
        Good ->
          let
            !name = Name.fromPtr pos newPos
            !newState = P.State src newPos end indent row newCol
          in
          cok name newState

        Bad ->
          cerr row newCol toError


data ModuleNameStatus
  = Good
  | Bad


moduleNameHelp :: Ptr Word8 -> Ptr Word8 -> Col -> (# ModuleNameStatus, Ptr Word8, Col #)
moduleNameHelp pos end col =
  if isDot pos end then
    let
      !pos1 = plusPtr pos 1
      (# newPos, newCol #) = chompUpper pos1 end (col + 1)
    in
    if pos1 == newPos then
      (# Bad, newPos, newCol #)
    else
      moduleNameHelp newPos end newCol

  else
    (# Good, pos, col #)



-- FOREIGN UPPER


data Upper
  = Unqualified Name.Name
  | Qualified Name.Name Name.Name


foreignUpper :: (Row -> Col -> x) -> Parser x Upper
foreignUpper toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    let (# upperStart, upperEnd, newCol #) = foreignUpperHelp pos end col in
    if upperStart == upperEnd then
      eerr row newCol toError
    else
      let
        !newState = P.State src upperEnd end indent row newCol
        !name = Name.fromPtr upperStart upperEnd
        !upperName =
          if upperStart == pos then
            Unqualified name
          else
            let !home = Name.fromPtr pos (plusPtr upperStart (-1)) in
            Qualified home name
      in
      cok upperName newState


foreignUpperHelp :: Ptr Word8 -> Ptr Word8 -> Col -> (# Ptr Word8, Ptr Word8, Col #)
foreignUpperHelp pos end col =
  let
    (# newPos, newCol #) = chompUpper pos end col
  in
  if pos == newPos then
    (# pos, pos, col #)

  else if isDot newPos end then
    foreignUpperHelp (plusPtr newPos 1) end (newCol + 1)

  else
    (# pos, newPos, newCol #)



-- FOREIGN ALPHA


foreignAlpha :: (Row -> Col -> x) -> Parser x Src.Expr_
foreignAlpha toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ _ eerr ->
    let (# alphaStart, alphaEnd, newCol, varType #) = foreignAlphaHelp pos end col in
    if alphaStart == alphaEnd then
      eerr row newCol toError
    else
      let
        !newState = P.State src alphaEnd end indent row newCol
        !name = Name.fromPtr alphaStart alphaEnd
      in
      if alphaStart == pos then
        if Set.member name reservedWords then
          eerr row col toError
        else
          cok (Src.Var varType name) newState
      else
        let !home = Name.fromPtr pos (plusPtr alphaStart (-1)) in
        cok (Src.VarQual varType home name) newState


foreignAlphaHelp :: Ptr Word8 -> Ptr Word8 -> Col -> (# Ptr Word8, Ptr Word8, Col, Src.VarType #)
foreignAlphaHelp pos end col =
  let
    (# lowerPos, lowerCol #) = chompLower pos end col
  in
  if pos < lowerPos then
    (# pos, lowerPos, lowerCol, Src.LowVar #)

  else
    let
      (# upperPos, upperCol #) = chompUpper pos end col
    in
    if pos == upperPos then
      (# pos, pos, col, Src.CapVar #)

    else if isDot upperPos end then
      foreignAlphaHelp (plusPtr upperPos 1) end (upperCol + 1)

    else
      (# pos, upperPos, upperCol, Src.CapVar #)



---- CHAR CHOMPERS ----



-- DOTS


{-# INLINE isDot #-}
isDot :: Ptr Word8 -> Ptr Word8 -> Bool
isDot pos end =
  pos < end && unsafeIndex pos == 0x2e {- . -}



-- UPPER CHARS


chompUpper :: Ptr Word8 -> Ptr Word8 -> Col -> (# Ptr Word8, Col #)
chompUpper pos end col =
  let !width = getUpperWidth pos end in
  if width == 0 then
    (# pos, col #)
  else
    chompInnerChars (plusPtr pos width) end (col + 1)


{-# INLINE getUpperWidth #-}
getUpperWidth :: Ptr Word8 -> Ptr Word8 -> Int
getUpperWidth pos end =
  if pos < end then
    getUpperWidthHelp pos end (unsafeIndex pos)
  else
    0


{-# INLINE getUpperWidthHelp #-}
getUpperWidthHelp :: Ptr Word8 -> Ptr Word8 -> Word8 -> Int
getUpperWidthHelp pos _ word
  | 0x41 {- A -} <= word && word <= 0x5A {- Z -} = 1
  | word < 0xc0 = 0
  | word < 0xe0 = if Char.isUpper (chr2 pos word) then 2 else 0
  | word < 0xf0 = if Char.isUpper (chr3 pos word) then 3 else 0
  | word < 0xf8 = if Char.isUpper (chr4 pos word) then 4 else 0
  | True        = 0



-- LOWER CHARS


chompLower :: Ptr Word8 -> Ptr Word8 -> Col -> (# Ptr Word8, Col #)
chompLower pos end col =
  let !width = getLowerWidth pos end in
  if width == 0 then
    (# pos, col #)
  else
    chompInnerChars (plusPtr pos width) end (col + 1)


{-# INLINE getLowerWidth #-}
getLowerWidth :: Ptr Word8 -> Ptr Word8 -> Int
getLowerWidth pos end =
  if pos < end then
    getLowerWidthHelp pos end (unsafeIndex pos)
  else
    0


{-# INLINE getLowerWidthHelp #-}
getLowerWidthHelp :: Ptr Word8 -> Ptr Word8 -> Word8 -> Int
getLowerWidthHelp pos _ word
  | 0x61 {- a -} <= word && word <= 0x7A {- z -} = 1
  | word < 0xc0 = 0
  | word < 0xe0 = if Char.isLower (chr2 pos word) then 2 else 0
  | word < 0xf0 = if Char.isLower (chr3 pos word) then 3 else 0
  | word < 0xf8 = if Char.isLower (chr4 pos word) then 4 else 0
  | True        = 0



-- INNER CHARS


chompInnerChars :: Ptr Word8 -> Ptr Word8 -> Col -> (# Ptr Word8, Col #)
chompInnerChars !pos end !col =
  let !width = getInnerWidth pos end in
  if width == 0 then
    (# pos, col #)
  else
    chompInnerChars (plusPtr pos width) end (col + 1)


getInnerWidth :: Ptr Word8 -> Ptr Word8 -> Int
getInnerWidth pos end =
  if pos < end then
    getInnerWidthHelp pos end (unsafeIndex pos)
  else
    0


{-# INLINE getInnerWidthHelp #-}
getInnerWidthHelp :: Ptr Word8 -> Ptr Word8 -> Word8 -> Int
getInnerWidthHelp pos _ word
  | 0x61 {- a -} <= word && word <= 0x7A {- z -} = 1
  | 0x41 {- A -} <= word && word <= 0x5A {- Z -} = 1
  | 0x30 {- 0 -} <= word && word <= 0x39 {- 9 -} = 1
  | word == 0x5F {- _ -} = 1
  | word < 0xc0 = 0
  | word < 0xe0 = if Char.isAlpha (chr2 pos word) then 2 else 0
  | word < 0xf0 = if Char.isAlpha (chr3 pos word) then 3 else 0
  | word < 0xf8 = if Char.isAlpha (chr4 pos word) then 4 else 0
  | True        = 0



-- EXTRACT CHARACTERS


{-# INLINE chr2 #-}
chr2 :: Ptr Word8 -> Word8 -> Char
chr2 pos firstWord =
  let
    !i1# = unpack firstWord
    !i2# = unpack (unsafeIndex (plusPtr pos 1))
    !c1# = uncheckedIShiftL# (i1# -# 0xC0#) 6#
    !c2# = i2# -# 0x80#
  in
  C# (chr# (c1# +# c2#))


{-# INLINE chr3 #-}
chr3 :: Ptr Word8 -> Word8 -> Char
chr3 pos firstWord =
  let
    !i1# = unpack firstWord
    !i2# = unpack (unsafeIndex (plusPtr pos 1))
    !i3# = unpack (unsafeIndex (plusPtr pos 2))
    !c1# = uncheckedIShiftL# (i1# -# 0xE0#) 12#
    !c2# = uncheckedIShiftL# (i2# -# 0x80#) 6#
    !c3# = i3# -# 0x80#
  in
  C# (chr# (c1# +# c2# +# c3#))


{-# INLINE chr4 #-}
chr4 :: Ptr Word8 -> Word8 -> Char
chr4 pos firstWord =
  let
    !i1# = unpack firstWord
    !i2# = unpack (unsafeIndex (plusPtr pos 1))
    !i3# = unpack (unsafeIndex (plusPtr pos 2))
    !i4# = unpack (unsafeIndex (plusPtr pos 3))
    !c1# = uncheckedIShiftL# (i1# -# 0xF0#) 18#
    !c2# = uncheckedIShiftL# (i2# -# 0x80#) 12#
    !c3# = uncheckedIShiftL# (i3# -# 0x80#) 6#
    !c4# = i4# -# 0x80#
  in
  C# (chr# (c1# +# c2# +# c3# +# c4#))


unpack :: Word8 -> Int#
unpack (W8# word#) =
  word2Int# (word8ToWord# word#)
