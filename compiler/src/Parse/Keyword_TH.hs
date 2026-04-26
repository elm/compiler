{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, TemplateHaskell #-}
module Parse.Keyword_TH
  ( keyword
  )
  where


import Control.Monad (foldM)
import qualified Data.Char as Char
import qualified Data.List as List
import GHC.Prim
import GHC.Word (Word8(..))
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Exp(..), Pat(..), Lit(..), newName)

import qualified AST.Prim.Variable as Var
import Parse.Primitives (Parser, Cursor, eqIndex, eqAddr, leAddr, slide)
import qualified Parse.Primitives as P



-- KEYWORD


keyword :: QuasiQuoter
keyword =
  QuasiQuoter
    { quoteExp  = keywordHelper
    , quotePat  = \_ -> fail "cannot use [keyword|...|] as a pattern"
    , quoteType = \_ -> fail "cannot use [keyword|...|] as a type"
    , quoteDec  = \_ -> fail "cannot use [keyword|...|] as a declaration"
    }
  where
    keywordHelper chars =
      case List.length chars of
        2  -> call 'k2  chars
        3  -> call 'k3  chars
        4  -> call 'k4  chars
        5  -> call 'k5  chars
        6  -> call 'k6  chars
        7  -> call 'k7  chars
        8  -> call 'k8  chars
        9  -> call 'k9  chars
        10 -> call 'k10 chars
        11 -> call 'k11 chars
        12 -> call 'k12 chars
        n  -> fail $ "cannot handle keywords of length " ++ show n

    call func args =
      do  x <- newName "x"
          f <- foldM apply (VarE func) args
          pure $ LamE [VarP x] (f `AppE` VarE x)

    apply func char =
      do  letter <- toLetter char
          pure $ func `AppE` LitE (IntegerL (fromIntegral letter))

    toLetter char =
      case Char.ord char of
        n | 0x41 <= n && n <= 0x5A -> pure n
          | 0x61 <= n && n <= 0x7A -> pure n
          | otherwise              -> fail "only ASCII upper and lower letters are allowed"



-- KEYWORD HELPERS
--
-- PERF consider reading Word16/Word32/Word64 to go a bit faster.


k2 :: Word8 -> Word8 -> (Cursor -> x) -> Parser x ()
k2 (W8# w1) (W8# w2) toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !pos2 = plusAddr# pos 2# in
    if leAddr pos2 end
      && eqIndex pos 0# w1
      && eqIndex pos 1# w2
      && isNotDirtyEnd pos2 end
    then
      let !s = P.State pos2 end indent (slide cur 2#Word64) in cok () s
    else
      eerr cur toError


k3 :: Word8 -> Word8 -> Word8 -> (Cursor -> x) -> Parser x ()
k3 (W8# w1) (W8# w2) (W8# w3) toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !pos3 = plusAddr# pos 3# in
    if leAddr pos3 end
      && eqIndex pos 0# w1
      && eqIndex pos 1# w2
      && eqIndex pos 2# w3
      && isNotDirtyEnd pos3 end
    then
      let !s = P.State pos3 end indent (slide cur 3#Word64) in cok () s
    else
      eerr cur toError


k4 :: Word8 -> Word8 -> Word8 -> Word8 -> (Cursor -> x) -> Parser x ()
k4 (W8# w1) (W8# w2) (W8# w3) (W8# w4) toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !pos4 = plusAddr# pos 4# in
    if leAddr pos4 end
      && eqIndex pos 0# w1
      && eqIndex pos 1# w2
      && eqIndex pos 2# w3
      && eqIndex pos 3# w4
      && isNotDirtyEnd pos4 end
    then
      let !s = P.State pos4 end indent (slide cur 4#Word64) in cok () s
    else
      eerr cur toError


k5 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Cursor -> x) -> Parser x ()
k5 (W8# w1) (W8# w2) (W8# w3) (W8# w4) (W8# w5) toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !pos5 = plusAddr# pos 5# in
    if leAddr pos5 end
      && eqIndex pos 0# w1
      && eqIndex pos 1# w2
      && eqIndex pos 2# w3
      && eqIndex pos 3# w4
      && eqIndex pos 4# w5
      && isNotDirtyEnd pos5 end
    then
      let !s = P.State pos5 end indent (slide cur 5#Word64) in cok () s
    else
      eerr cur toError


k6 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Cursor -> x) -> Parser x ()
k6 (W8# w1) (W8# w2) (W8# w3) (W8# w4) (W8# w5) (W8# w6) toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !pos6 = plusAddr# pos 6# in
    if leAddr pos6 end
      && eqIndex pos 0# w1
      && eqIndex pos 1# w2
      && eqIndex pos 2# w3
      && eqIndex pos 3# w4
      && eqIndex pos 4# w5
      && eqIndex pos 5# w6
      && isNotDirtyEnd pos6 end
    then
      let !s = P.State pos6 end indent (slide cur 6#Word64) in cok () s
    else
      eerr cur toError


k7 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Cursor -> x) -> Parser x ()
k7 (W8# w1) (W8# w2) (W8# w3) (W8# w4) (W8# w5) (W8# w6) (W8# w7) toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !pos7 = plusAddr# pos 7# in
    if leAddr pos7 end
      && eqIndex pos 0# w1
      && eqIndex pos 1# w2
      && eqIndex pos 2# w3
      && eqIndex pos 3# w4
      && eqIndex pos 4# w5
      && eqIndex pos 5# w6
      && eqIndex pos 6# w7
      && isNotDirtyEnd pos7 end
    then
      let !s = P.State pos7 end indent (slide cur 7#Word64) in cok () s
    else
      eerr cur toError


k8 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Cursor -> x) -> Parser x ()
k8 (W8# w1) (W8# w2) (W8# w3) (W8# w4) (W8# w5) (W8# w6) (W8# w7) (W8# w8) toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !pos8 = plusAddr# pos 8# in
    if leAddr pos8 end
      && eqIndex pos 0# w1
      && eqIndex pos 1# w2
      && eqIndex pos 2# w3
      && eqIndex pos 3# w4
      && eqIndex pos 4# w5
      && eqIndex pos 5# w6
      && eqIndex pos 6# w7
      && eqIndex pos 7# w8
      && isNotDirtyEnd pos8 end
    then
      let !s = P.State pos8 end indent (slide cur 8#Word64) in cok () s
    else
      eerr cur toError


k9 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Cursor -> x) -> Parser x ()
k9 (W8# w1) (W8# w2) (W8# w3) (W8# w4) (W8# w5) (W8# w6) (W8# w7) (W8# w8) (W8# w9) toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !pos9 = plusAddr# pos 9# in
    if leAddr pos9 end
      && eqIndex pos 0# w1
      && eqIndex pos 1# w2
      && eqIndex pos 2# w3
      && eqIndex pos 3# w4
      && eqIndex pos 4# w5
      && eqIndex pos 5# w6
      && eqIndex pos 6# w7
      && eqIndex pos 7# w8
      && eqIndex pos 8# w9
      && isNotDirtyEnd pos9 end
    then
      let !s = P.State pos9 end indent (slide cur 9#Word64) in cok () s
    else
      eerr cur toError


k10 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Cursor -> x) -> Parser x ()
k10 (W8# w1) (W8# w2) (W8# w3) (W8# w4) (W8# w5) (W8# w6) (W8# w7) (W8# w8) (W8# w9) (W8# w10) toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !pos10 = plusAddr# pos 10# in
    if leAddr pos10 end
      && eqIndex pos 0# w1
      && eqIndex pos 1# w2
      && eqIndex pos 2# w3
      && eqIndex pos 3# w4
      && eqIndex pos 4# w5
      && eqIndex pos 5# w6
      && eqIndex pos 6# w7
      && eqIndex pos 7# w8
      && eqIndex pos 8# w9
      && eqIndex pos 9# w10
      && isNotDirtyEnd pos10 end
    then
      let !s = P.State pos10 end indent (slide cur 10#Word64) in cok () s
    else
      eerr cur toError


k11 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Cursor -> x) -> Parser x ()
k11 (W8# w1) (W8# w2) (W8# w3) (W8# w4) (W8# w5) (W8# w6) (W8# w7) (W8# w8) (W8# w9) (W8# w10) (W8# w11) toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !pos11 = plusAddr# pos 11# in
    if leAddr pos11 end
      && eqIndex pos  0# w1
      && eqIndex pos  1# w2
      && eqIndex pos  2# w3
      && eqIndex pos  3# w4
      && eqIndex pos  4# w5
      && eqIndex pos  5# w6
      && eqIndex pos  6# w7
      && eqIndex pos  7# w8
      && eqIndex pos  8# w9
      && eqIndex pos  9# w10
      && eqIndex pos 10# w11
      && isNotDirtyEnd pos11 end
    then
      let !s = P.State pos11 end indent (slide cur 11#Word64) in cok () s
    else
      eerr cur toError


k12 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> (Cursor -> x) -> Parser x ()
k12 (W8# w1) (W8# w2) (W8# w3) (W8# w4) (W8# w5) (W8# w6) (W8# w7) (W8# w8) (W8# w9) (W8# w10) (W8# w11) (W8# w12) toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !pos12 = plusAddr# pos 12# in
    if leAddr pos12 end
      && eqIndex pos  0# w1
      && eqIndex pos  1# w2
      && eqIndex pos  2# w3
      && eqIndex pos  3# w4
      && eqIndex pos  4# w5
      && eqIndex pos  5# w6
      && eqIndex pos  6# w7
      && eqIndex pos  7# w8
      && eqIndex pos  8# w9
      && eqIndex pos  9# w10
      && eqIndex pos 10# w11
      && eqIndex pos 11# w12
      && isNotDirtyEnd pos12 end
    then
      let !s = P.State pos12 end indent (slide cur 12#Word64) in cok () s
    else
      eerr cur toError



-- HELPERS


isNotDirtyEnd :: Addr# -> Addr# -> Bool
isNotDirtyEnd pos end =
  eqAddr pos end
  ||
  eqAddr pos (Var.chompInner pos end (indexWord8OffAddr# pos 0#))

