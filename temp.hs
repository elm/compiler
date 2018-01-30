{-# LANGUAGE OverloadedStrings #-}
module Temp
  ( toByte
  , intToAscii
  )
  where


import qualified Data.ByteString.Short as S
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)



-- RESERVED NAMES


jsReservedWords :: Set.Set Text.Text
jsReservedWords =
  Set.fromList
    [ "do", "if", "in"
    , "NaN", "int", "for", "new", "try", "var", "let"
    , "null", "true", "eval", "byte", "char", "goto", "long", "case", "else", "this", "void", "with", "enum"
    , "false", "final", "float", "short", "break", "catch", "throw", "while", "class", "const", "super", "yield"
    , "double", "native", "throws", "delete", "return", "switch", "typeof", "export", "import", "public", "static"
    , "boolean", "default", "finally", "extends", "package", "private"
    , "Infinity", "abstract", "volatile", "function", "continue", "debugger", "function"
    , "undefined", "arguments", "transient", "interface", "protected"
    , "instanceof", "implements"
    , "synchronized"
    ]



-- INT TO ASCII


intToAscii :: Int -> S.ShortByteString
intToAscii n =
  if n < 53 then -- skip $ as a standalone name
    S.pack [toByte n]

  else
    intToAsciiHelp (numStartBytes * numInnerBytes) allBadFields (n - 53)


intToAsciiHelp :: Int -> [BadFields] -> Int -> S.ShortByteString
intToAsciiHelp blockSize badFields n =
  case badFields of
    [] ->
      if n < blockSize then
        unsafeIntToAscii n
      else
        intToAsciiHelp (blockSize * numInnerBytes) [] (n - blockSize)

    BadFields renamings : biggerBadFields ->
      let availableSize = blockSize - Map.size renamings in
      if n < availableSize then
        let name = unsafeIntToAscii n in
        Map.findWithDefault name name renamings
      else
        intToAsciiHelp (blockSize * numInnerBytes) biggerBadFields (n - availableSize)



-- UNSAFE INT TO ASCII


unsafeIntToAscii :: Int -> S.ShortByteString
unsafeIntToAscii n =
  let
    (quotient, remainder) =
      quotRem n numStartBytes
  in
  S.pack $ toByte remainder : unsafeIntToAsciiHelp quotient


unsafeIntToAsciiHelp :: Int -> [Word8]
unsafeIntToAsciiHelp n =
  if n == 0 then
    []
  else
    let (quotient, remainder) = quotRem n numInnerBytes in
    toByte remainder : unsafeIntToAsciiHelp quotient



-- ASCII BYTES


numStartBytes :: Int
numStartBytes =
  54


numInnerBytes :: Int
numInnerBytes =
  64


toByte :: Int -> Word8
toByte n
  | n < 26  = fromIntegral (97 + n     ) {- lower -}
  | n < 52  = fromIntegral (65 + n - 26) {- upper -}
  | n == 52 = 0x5F {- _ -}
  | n == 53 = 0x24 {- $ -}
  | n < 64  = fromIntegral (48 + n - 54) {- digit -}
  | True    = error $ "cannot convert int " ++ show n ++ " to ASCII"



-- BAD FIELDS


newtype BadFields =
  BadFields { _renamings :: Renamings }


type Renamings =
  Map.Map S.ShortByteString S.ShortByteString


allBadFields :: [BadFields]
allBadFields =
  let
    add keyword dict =
      Map.alter (Just . addRenaming keyword) (Text.length keyword) dict
  in
    Map.elems $ Set.foldr add Map.empty jsReservedWords


addRenaming :: Text.Text -> Maybe BadFields -> BadFields
addRenaming keyword maybeBadFields =
  let
    maxName =
      numStartBytes * numInnerBytes ^ (Text.length keyword - 1) - 1
  in
  case maybeBadFields of
    Nothing ->
      BadFields $ Map.singleton (S.toShort (Text.encodeUtf8 keyword)) (unsafeIntToAscii maxName)

    Just (BadFields renamings) ->
      BadFields $ Map.insert (S.toShort (Text.encodeUtf8 keyword)) (unsafeIntToAscii (maxName - Map.size renamings)) renamings
