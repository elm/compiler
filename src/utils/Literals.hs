{-# LANGUAGE BangPatterns, MagicHash, TemplateHaskell, UnboxedTuples #-}
module Literals
  ( s
  , string
  --
  , b
  , builder
  , builderHelp
  , toBuilderHelp
  --
  , file
  --
  , Table(..)
  , table
  , read
  --
  , FSM(..)
  , fsm
  , step
  , steps
  )
  where


import Prelude hiding (read)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Builder.Internal as B
import qualified Data.List as List
import Data.Bits ((.|.), (.&.), unsafeShiftR)
import Data.Char (ord)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import GHC.Exts (isTrue#)
import GHC.ForeignPtr (ForeignPtr(ForeignPtr), ForeignPtrContents(FinalPtr))
import GHC.IO (IO(IO))
import GHC.Prim
import GHC.Ptr (Ptr(Ptr))
import GHC.Word (Word8(..))
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Q, Exp(..), Lit(..), Bytes(Bytes), qAddDependentFile, runIO)



-- MULTI-LINE STRINGS


s :: QuasiQuoter
s =
  QuasiQuoter
    { quoteExp  = return . LitE . StringL
    , quotePat  = \_ -> fail "cannot use [s| ... |] as a pattern"
    , quoteType = \_ -> fail "cannot use [s| ... |] as a type"
    , quoteDec  = \_ -> fail "cannot use [s| ... |] as a declaration"
    }


string :: QuasiQuoter
string =
  QuasiQuoter
    { quoteExp  = return . LitE . StringL
    , quotePat  = \_ -> fail "cannot use [string| ... |] as a pattern"
    , quoteType = \_ -> fail "cannot use [string| ... |] as a type"
    , quoteDec  = \_ -> fail "cannot use [string| ... |] as a declaration"
    }



-- BUILDERS
--
-- The default OverloadedStrings for Builders is not that good, so this
-- guarantees that you get the fastest possible builder for a string literal.


b :: QuasiQuoter
b =
  QuasiQuoter
    { quoteExp  = builderHelp
    , quotePat  = \_ -> fail "cannot use [b| ... |] as a pattern"
    , quoteType = \_ -> fail "cannot use [b| ... |] as a type"
    , quoteDec  = \_ -> fail "cannot use [b| ... |] as a declaration"
    }


builder :: QuasiQuoter
builder =
  QuasiQuoter
    { quoteExp  = builderHelp
    , quotePat  = \_ -> fail "cannot use [builder| ... |] as a pattern"
    , quoteType = \_ -> fail "cannot use [builder| ... |] as a type"
    , quoteDec  = \_ -> fail "cannot use [builder| ... |] as a declaration"
    }


builderHelp :: [Char] -> Q Exp
builderHelp str =
  do  bytes <- go str
      let len = IntPrimL $ fromIntegral $ List.length bytes
      return $ VarE 'toBuilder `AppE` LitE (StringPrimL bytes) `AppE` LitE len
  where
    go chars =
      case chars of
        [] ->
          pure []

        char:rest
          | code == 0x5C ->
              case rest of
                'n' :cs -> 0x0A <:> go cs
                'r' :cs -> 0x0D <:> go cs
                't' :cs -> 0x09 <:> go cs
                '\\':cs -> 0x5C <:> go cs
                'x' :cs -> goHex cs
                _       -> fail "uses unknown escape, check your backslashes"

          | code <= 0x7F ->
              code <:> go rest

          | code <= 0x7FF ->
              0xC0 .|.          unsafeShiftR code 6 <:>
              0x80 .|. 0x3F .&.              code   <:> go rest

          | code <= 0xFFFF ->
              0xE0 .|.          unsafeShiftR code 12 <:>
              0x80 .|. 0x3F .&. unsafeShiftR code  6 <:>
              0x80 .|. 0x3F .&.              code    <:> go rest

          | code <= 0x10FFFF ->
              0xF0 .|.          unsafeShiftR code 18 <:>
              0x80 .|. 0x3F .&. unsafeShiftR code 12 <:>
              0x80 .|. 0x3F .&. unsafeShiftR code  6 <:>
              0x80 .|. 0x3F .&.              code    <:> go rest

          | otherwise ->
              fail $ "code point (" ++ show code ++ ") outside of Unicode range"
          where
            code = ord char

    goHex chars =
      case chars of
        c:cs
          | isDigit c -> goHex2 (ord c - 48) cs
          | isLower c -> goHex2 (ord c - 87) cs
          | isUpper c -> goHex2 (ord c - 55) cs

        _ ->
          fail "invalid hex escape"

    goHex2 x chars =
      case chars of
        c:cs
          | isDigit c -> 16 * x + (ord c - 48) <:> go cs
          | isLower c -> 16 * x + (ord c - 87) <:> go cs
          | isUpper c -> 16 * x + (ord c - 55) <:> go cs

        _ ->
          fail "invalid hex escape"


infixr 1 <:>

(<:>) :: Int -> Q [Word8] -> Q [Word8]
(<:>) n qRest =
  (:) (fromIntegral n) <$> qRest


{-# INLINE toBuilder #-}
toBuilder :: Addr# -> Int# -> B.Builder
toBuilder =
  \addr len -> B.builder (toBuilderHelp addr len)


{-# INLINE toBuilderHelp #-}
toBuilderHelp :: Addr# -> Int# -> B.BuildStep a -> B.BuildStep a
toBuilderHelp start size k =
    go start size
  where
    go src len (B.BufferRange (Ptr dst) (Ptr end)) =
      let
        space = minusAddr# end dst
      in
      if isTrue# (len <=# space) then
        do  IO $ \s0 -> case copyAddrToAddrNonOverlapping# src dst len s0 of { s1 -> (# s1, () #) }
            let !br = B.BufferRange (Ptr (plusAddr# dst len)) (Ptr end)
            k br
      else
        do  IO $ \s0 -> case copyAddrToAddrNonOverlapping# src dst space s0 of { s1 -> (# s1, () #) }
            return $ B.bufferFull 1 (Ptr end) (go (plusAddr# src space) (len -# space))



-- FILES


file :: FilePath -> Q Exp
file path =
  do  qAddDependentFile path
      (BS.BS fptr len) <- runIO $ BS.readFile path
      let bytes = Bytes fptr 0 (fromIntegral len)
      return $
        ConE 'BS.BS
          `AppE` (ConE 'ForeignPtr `AppE` LitE (BytesPrimL bytes) `AppE` ConE 'FinalPtr)
          `AppE` LitE (IntegerL (fromIntegral len))



-- TABLE


data Table =
  Table Addr#


table :: QuasiQuoter
table =
  QuasiQuoter
    { quoteExp  = spaces []
    , quotePat  = \_ -> fail "cannot use [table| ... |] as a pattern"
    , quoteType = \_ -> fail "cannot use [table| ... |] as a type"
    , quoteDec  = \_ -> fail "cannot use [table| ... |] as a declaration"
    }
  where
    spaces revs chars =
      case chars of
        [] ->
          done revs

        c:cs
          | c == ' '  -> spaces revs cs
          | c == '\n' -> spaces revs cs
          | isDigit c -> digits (ord c - 48) revs cs
          | isLower c -> digits (ord c - 87) revs cs
          | isUpper c -> digits (ord c - 55) revs cs
          | c == '_'  -> blanks revs cs
          | otherwise -> fail $ "ran into unexpected chars (" ++ show c ++ ")"

    digits n revs chars =
      case chars of
        [] ->
          done (Just n : revs)

        c:cs
          | c == ' '  -> spaces (Just n : revs) cs
          | c == '\n' -> spaces (Just n : revs) cs
          | isDigit c -> digits (16 * n + (ord c - 48)) revs cs
          | isLower c -> digits (16 * n + (ord c - 87)) revs cs
          | isUpper c -> digits (16 * n + (ord c - 55)) revs cs
          | otherwise -> fail $ "ran into unexpected character (" ++ show c ++ ")"

    blanks revs chars =
      case chars of
        [] ->
          done (Nothing : revs)

        c:cs
          | c == ' '  -> spaces (Nothing : revs) cs
          | c == '\n' -> spaces (Nothing : revs) cs
          | c == '_'  -> blanks revs cs
          | otherwise -> fail $ "ran into unexpected character (" ++ show c ++ ")"

    done revs =
      let
        highest  = List.maximum (catMaybes revs)
        replacer = highest + 1
      in
      if List.length revs == 256
      then
        if replacer < 256 || (highest < 256 && List.all isJust revs)
        then
          do  let (BS.BS fptr len) = BS.pack (List.map (fromIntegral . fromMaybe replacer) (List.reverse revs))
              let bytes = Bytes fptr 0 (fromIntegral len)
              return $ AppE (ConE 'Table) (LitE (BytesPrimL bytes))
        else
          fail "expecting all entries to be under 256"
      else
        fail "expecting exactly 256 entries"



{-# INLINE isDigit #-}
{-# INLINE isLower #-}
{-# INLINE isUpper #-}

isDigit :: Char -> Bool
isLower :: Char -> Bool
isUpper :: Char -> Bool

isDigit c = '0' <= c && c <= '9'
isLower c = 'a' <= c && c <= 'f'
isUpper c = 'A' <= c && c <= 'F'


read :: Addr# -> Word8# -> Word8#
read tbl word =
  indexWord8OffAddr# tbl (word2Int# (word8ToWord# word))



-- FINITE STATE MACHINE


data FSM =
  FSM Addr#


fsm :: QuasiQuoter
fsm =
  QuasiQuoter
    { quoteExp  = \str -> loop1 str []
    , quotePat  = \_ -> fail "cannot use [fsm| ... |] as a pattern"
    , quoteType = \_ -> fail "cannot use [fsm| ... |] as a type"
    , quoteDec  = \_ -> fail "cannot use [fsm| ... |] as a declaration"
    }
  where
    loop1 chars rtable =
      case chars of
        [] ->
          fail "expecting a -------- and then the state table"

        c:cs
          | c == ' '             -> loop1 cs rtable
          | c == '\n'            -> loop1 cs rtable
          | '0' <= c && c <= '9' -> loop1 cs (ord c - 48 : rtable)
          | 'a' <= c && c <= 'f' -> loop1 cs (ord c - 87 : rtable)
          | 'A' <= c && c <= 'F' -> loop1 cs (ord c - 55 : rtable)
          | c == '-'             -> loop2 cs rtable
          | otherwise            -> fail $ "ran into unexpected character (" ++ show c ++ ")"

    loop2 chars rtable =
      case chars of
        [] ->
          fail "expecting the state table after the --------"

        c:cs
          | c == '-'  -> loop2 cs rtable
          | otherwise -> loop3 cs rtable [] []

    loop3 chars rtable rcols rrows =
      case chars of
        [] ->
          check rtable (rcols:rrows)

        c:cs
          | c == ' '             -> loop3 cs rtable rcols rrows
          | c == '\n'            -> loop3 cs rtable [] (rcols:rrows)
          | '0' <= c && c <= '9' -> loop3 cs rtable (ord c - 48 : rcols) rrows
          | 'a' <= c && c <= 'f' -> loop3 cs rtable (ord c - 87 : rcols) rrows
          | 'A' <= c && c <= 'F' -> loop3 cs rtable (ord c - 55 : rcols) rrows
          | otherwise            -> fail $ "ran into unexpected character (" ++ show c ++ ")"

    check rtable rrows =
      do  tbl  <- checkTable rtable
          rows <- checkRows  rrows
          let (BS.BS fptr len) = BS.pack (tbl ++ rows)
          let bytes = Bytes fptr 0 (fromIntegral len)
          return $ AppE (ConE 'FSM) (LitE (BytesPrimL bytes))

    checkTable rtable =
      if List.length rtable == 256
      then return (List.map fromIntegral (List.reverse rtable))
      else fail "expecting exactly 256 entries before the --------"

    checkRows rrows =
      case List.reverse (List.filter (not . List.null) rrows) of
        [] ->
          fail "expecting the state table after the --------"

        row:rows ->
          let
            len = List.length row
          in
          if List.all (\r -> len == List.length r) rows
          then traverse (checkColumn len) (List.concatMap List.reverse (row:rows))
          else fail "expecting all rows in the state table to have the same number of columns"

    checkColumn len n =
      let
        col = n * len
      in
      if col < 256
      then return (fromIntegral col)
      else fail "ran into a value over 256 in the state table"


{-# INLINE step #-}
step :: Addr# -> Word8# -> Word8# -> Word8#
step tbl state word =
  let
    !tran = indexInt8OffAddr# tbl (word2Int# (word8ToWord# word))
  in
  indexWord8OffAddr# tbl (256# +# word2Int# (word8ToWord# state) +# int8ToInt# tran)


{-# INLINE steps #-}
steps :: FSM -> Addr# -> Addr# -> Word8# -> Word8#
steps machine start end s0 =
    loop start s0
  where
    !(FSM tbl) = machine

    loop pos state =
      if isTrue# (ltAddr# pos end)
      then loop (plusAddr# pos 1#) (step tbl state (indexWord8OffAddr# pos 0#))
      else state

