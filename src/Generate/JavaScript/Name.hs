{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Name
  ( Name
  , Mode(..)
  , toBuilder
  , fromInt
  , fromLocal
  , fromGlobal
  , fromCycle
  , fromKernel
  , fromField
  , makeLabel
  , makeTemp
  , dollar
  )
  where


import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Short as S
import Data.Monoid ((<>))
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)

import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Elm.Package as Pkg



-- NAME


newtype Name =
  Name { toBuilder :: B.Builder }



-- MODE


data Mode
  = Debug
  | Prod (Map.Map N.Name B.Builder)



-- CONSTRUCTORS


fromInt :: Int -> Name
fromInt n =
  Name (B.shortByteString (intToAscii n))


fromLocal :: N.Name -> Name
fromLocal name =
  if Set.member name reservedNames then
    Name ("_" <> N.toBuilder name)
  else
    Name (N.toBuilder name)


fromGlobal :: ModuleName.Canonical -> N.Name -> Name
fromGlobal (ModuleName.Canonical (Pkg.Name user project) home) name =
  Name $
    Text.encodeUtf8Builder (Text.replace "-" "_" user)
    <> "$" <> Text.encodeUtf8Builder (Text.replace "-" "_" project) -- TODO store this in a better way
    <> "$" <> N.toBuilder home
    <> "$" <> N.toBuilder name


fromCycle :: ModuleName.Canonical -> N.Name -> Name
fromCycle (ModuleName.Canonical (Pkg.Name user project) home) name =
  Name $
    Text.encodeUtf8Builder (Text.replace "-" "_" user)
    <> "$" <> Text.encodeUtf8Builder (Text.replace "-" "_" project) -- TODO store this in a better way
    <> "$" <> N.toBuilder home
    <> "$cyclic$" <> N.toBuilder name


fromKernel :: N.Name -> N.Name -> Name
fromKernel home name =
  Name ("_" <> N.toBuilder home <> "_" <> N.toBuilder name)


fromField :: Mode -> N.Name -> Name
fromField mode name =
  case mode of
    Debug ->
      Name (N.toBuilder name)

    Prod fields ->
      Name (fields ! name)



-- TEMPORARY NAMES


makeLabel :: N.Name -> Int -> Name
makeLabel name index =
  Name (N.toBuilder name <> "$" <> B.intDec index)


makeTemp :: N.Name -> Name
makeTemp name =
  Name ("$temp$" <> N.toBuilder name)


dollar :: Name
dollar =
  Name (N.toBuilder N.dollar)



-- RESERVED NAMES


{-# NOINLINE reservedNames #-}
reservedNames :: Set.Set N.Name
reservedNames =
  Set.union jsReservedWords elmReservedWords


jsReservedWords :: Set.Set N.Name
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


elmReservedWords :: Set.Set N.Name
elmReservedWords =
  Set.fromList
    [ "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9"
    , "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"
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
  | n == 53 = 95 {- _ -}
  | n == 54 = 36 {- $ -}
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
      Map.alter (Just . addRenaming keyword) (N.length keyword) dict
  in
    Map.elems $ Set.foldr add Map.empty jsReservedWords


addRenaming :: N.Name -> Maybe BadFields -> BadFields
addRenaming keyword maybeBadFields =
  let
    maxName =
      numStartBytes * numInnerBytes ^ (N.length keyword - 1) - 1
  in
  case maybeBadFields of
    Nothing ->
      BadFields $ Map.singleton (N.toShort keyword) (unsafeIntToAscii maxName)

    Just (BadFields renamings) ->
      BadFields $ Map.insert (N.toShort keyword) (unsafeIntToAscii (maxName - Map.size renamings)) renamings
