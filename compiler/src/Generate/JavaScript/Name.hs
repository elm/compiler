{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Name
  ( Name
  , toBuilder
  , fromIndex
  , fromInt
  , fromLocal
  , fromGlobal
  , fromCycle
  , fromKernel
  , makeF
  , makeA
  , makeLabel
  , makeTemp
  , dollar
  )
  where


import qualified Data.ByteString.Builder as B
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import Data.Word (Word8)

import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg



-- NAME


newtype Name =
  Name { toBuilder :: B.Builder }



-- CONSTRUCTORS


fromIndex :: Index.ZeroBased -> Name
fromIndex index =
  fromInt (Index.toMachine index)


fromInt :: Int -> Name
fromInt n =
  Name (Name.toBuilder (intToAscii n))


fromLocal :: Name.Name -> Name
fromLocal name =
  if Set.member name reservedNames then
    Name ("_" <> Name.toBuilder name)
  else
    Name (Name.toBuilder name)


fromGlobal :: ModuleName.Canonical -> Name.Name -> Name
fromGlobal home name =
  Name $ homeToBuilder home <> usd <> Name.toBuilder name


fromCycle :: ModuleName.Canonical -> Name.Name -> Name
fromCycle home name =
  Name $ homeToBuilder home <> "$cyclic$" <> Name.toBuilder name


fromKernel :: Name.Name -> Name.Name -> Name
fromKernel home name =
  Name ("_" <> Name.toBuilder home <> "_" <> Name.toBuilder name)


{-# INLINE homeToBuilder #-}
homeToBuilder :: ModuleName.Canonical -> B.Builder
homeToBuilder (ModuleName.Canonical (Pkg.Name author project) home) =
  usd <>
  Utf8.toEscapedBuilder 0x2D {- - -} 0x5F {- _ -} author
  <> usd <>
  Utf8.toEscapedBuilder 0x2D {- - -} 0x5F {- _ -} project
  <> usd <>
  Utf8.toEscapedBuilder 0x2E {- . -} 0x24 {- $ -} home



-- TEMPORARY NAMES


makeF :: Int -> Name
makeF n =
  Name ("F" <> B.intDec n)


makeA :: Int -> Name
makeA n =
  Name ("A" <> B.intDec n)


makeLabel :: Name.Name -> Int -> Name
makeLabel name index =
  Name (Name.toBuilder name <> usd <> B.intDec index)


makeTemp :: Name.Name -> Name
makeTemp name =
  Name ("$temp$" <> Name.toBuilder name)


dollar :: Name
dollar =
  Name usd


usd :: B.Builder
usd =
  Name.toBuilder Name.dollar



-- RESERVED NAMES


{-# NOINLINE reservedNames #-}
reservedNames :: Set.Set Name.Name
reservedNames =
  Set.union jsReservedWords elmReservedWords


jsReservedWords :: Set.Set Name.Name
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


elmReservedWords :: Set.Set Name.Name
elmReservedWords =
  Set.fromList
    [ "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9"
    , "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"
    ]



-- INT TO ASCII


intToAscii :: Int -> Name.Name
intToAscii n =
  if n < 53 then -- skip $ as a standalone name
    Name.fromWords [toByte n]

  else
    intToAsciiHelp 2 (numStartBytes * numInnerBytes) allBadFields (n - 53)


intToAsciiHelp :: Int -> Int -> [BadFields] -> Int -> Name.Name
intToAsciiHelp width blockSize badFields n =
  case badFields of
    [] ->
      if n < blockSize then
        unsafeIntToAscii width [] n
      else
        intToAsciiHelp (width + 1) (blockSize * numInnerBytes) [] (n - blockSize)

    BadFields renamings : biggerBadFields ->
      let availableSize = blockSize - Map.size renamings in
      if n < availableSize then
        let name = unsafeIntToAscii width [] n in
        Map.findWithDefault name name renamings
      else
        intToAsciiHelp (width + 1) (blockSize * numInnerBytes) biggerBadFields (n - availableSize)



-- UNSAFE INT TO ASCII


unsafeIntToAscii :: Int -> [Word8] -> Int -> Name.Name
unsafeIntToAscii width bytes n =
  if width <= 1 then
    Name.fromWords (toByte n : bytes)
  else
    let
      (quotient, remainder) =
        quotRem n numInnerBytes
    in
    unsafeIntToAscii (width - 1) (toByte remainder : bytes) quotient



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
  | n == 52 = 95 {- _ -}
  | n == 53 = 36 {- $ -}
  | n < 64  = fromIntegral (48 + n - 54) {- digit -}
  | True    = error $ "cannot convert int " ++ show n ++ " to ASCII"



-- BAD FIELDS


newtype BadFields =
  BadFields { _renamings :: Renamings }


type Renamings =
  Map.Map Name.Name Name.Name


allBadFields :: [BadFields]
allBadFields =
  let
    add keyword dict =
      Map.alter (Just . addRenaming keyword) (Utf8.size keyword) dict
  in
    Map.elems $ Set.foldr add Map.empty jsReservedWords


addRenaming :: Name.Name -> Maybe BadFields -> BadFields
addRenaming keyword maybeBadFields =
  let
    width = Utf8.size keyword
    maxName = numStartBytes * numInnerBytes ^ (width - 1) - 1
  in
  case maybeBadFields of
    Nothing ->
      BadFields $ Map.singleton keyword (unsafeIntToAscii width [] maxName)

    Just (BadFields renamings) ->
      BadFields $ Map.insert keyword (unsafeIntToAscii width [] (maxName - Map.size renamings)) renamings
