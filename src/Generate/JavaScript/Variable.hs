{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Variable
  ( Generator
  , run
  , fresh
  , local
  , safe
  , global
  , globalName
  , globalToName
  , intToAscii
  )
  where


import qualified Control.Monad.State.Strict as State
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (encodeUtf8)
import Data.Word (Word8)

import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Elm.Package as Pkg
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Helpers as JS



-- GENERATOR


type Generator = State.State Int


run :: Generator a -> a
run generator =
  State.evalState generator 0



-- FRESH NAMES


fresh :: Generator Text
fresh =
  do  uid <- State.get
      State.put (uid + 1)
      return ("_v" <> Text.pack (show uid))



-- LOCAL NAMES


local :: Text -> JS.Expr
local name =
  JS.ref (safe name)


safe :: Text -> Text
safe name =
  if Set.member name reservedNames then
    "_" <> name
  else
    name



-- RESERVED NAMES


reservedNames :: Set.Set Text
reservedNames =
  Set.union jsReservedWords elmReservedWords


jsReservedWords :: Set.Set Text
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


elmReservedWords :: Set.Set Text
elmReservedWords =
  Set.fromList
    [ "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9"
    , "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"
    ]



-- GLOBAL NAMES


global :: Var.Global -> Generator JS.Expr
global var =
  JS.ref <$> globalName var


globalName :: Var.Global -> Generator Text
globalName (Var.Global home name) =
  return (globalToName home name)


globalToName :: ModuleName.Canonical -> Text -> Text
globalToName (ModuleName.Canonical (Pkg.Name user project) moduleName) name =
  if ModuleName.isKernel moduleName then
    "_" <> ModuleName.getKernel moduleName <> "_" <> name

  else
    Text.replace "-" "_" user
    <> "$" <> Text.replace "-" "_" project
    <> "$" <> Text.replace "." "_" moduleName
    <> "$" <> name



-- INT TO ASCII


intToAscii :: Int -> BS.ByteString
intToAscii n =
  if n < 53 then -- skip $ as a standalone name
    BS.singleton (toByte n)

  else
    intToAsciiHelp (numStartBytes * numInnerBytes) allBadFields (n - 53)


intToAsciiHelp :: Int -> [BadFields] -> Int -> BS.ByteString
intToAsciiHelp blockSize badFields n =
  case badFields of
    [] ->
      if n < blockSize then
        unsafeIntToAscii n
      else
        intToAsciiHelp (blockSize * numInnerBytes) [] (n - blockSize)

    BadFields renamings : biggerBadFields ->
      if n < blockSize - Map.size renamings then
        let name = unsafeIntToAscii n in
        Map.findWithDefault name name renamings
      else
        intToAsciiHelp (blockSize * numInnerBytes) biggerBadFields (n - blockSize)



-- UNSAFE INT TO ASCII


unsafeIntToAscii :: Int -> BS.ByteString
unsafeIntToAscii n =
  let
    (quotient, remainder) =
      quotRem n numStartBytes
  in
    BS.pack $ toByte remainder : unsafeIntToAsciiHelp quotient


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
  Map.Map BS.ByteString BS.ByteString


allBadFields :: [BadFields]
allBadFields =
  let
    add keyword dict =
      Map.alter (Just . addRenaming keyword) (Text.length keyword) dict
  in
      Map.elems $ Set.foldr add Map.empty jsReservedWords


addRenaming :: Text -> Maybe BadFields -> BadFields
addRenaming keyword maybeBadFields =
  let
    maxName =
      numStartBytes * numInnerBytes ^ (Text.length keyword - 1) - 1
  in
  case maybeBadFields of
    Nothing ->
      BadFields $ Map.singleton (Text.encodeUtf8 keyword) (unsafeIntToAscii maxName)

    Just (BadFields renamings) ->
      BadFields $ Map.insert (Text.encodeUtf8 keyword) (unsafeIntToAscii (maxName - Map.size renamings)) renamings
