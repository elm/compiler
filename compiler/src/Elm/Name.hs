{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Name
  ( Name
  -- utilities
  , length
  , contains
  , startsWith
  , drop
  -- conversions
  , toText
  , toString
  , toBuilder
  , toDotlessBuilder
  , toShort
  -- helpers
  , addIndex
  , addSafeIndex
  , toCompositeName
  , sepBy
  -- create
  , fromForeignPtr
  , fromString
  , fromLetter
  , fromText
  -- interned
  , int, float, bool, char, string
  , maybe, result, list, array, dict, tuple, jsArray
  , task, router, cmd, sub, platform, virtualDom
  , shader, debug, debugger, bitwise, basics
  , utils, negate, true, false, value
  , node, program, main, dollar, identity
  , replModule
  )
  where


import Prelude hiding (drop, length, maybe, negate)
import Data.Binary
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Short as S
import qualified Data.Char as Char
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)



-- NAME


newtype Name = Name { _name :: Text.Text }
  deriving (Eq, Ord)


length :: Name -> Int
length (Name name) =
  Text.length name


contains :: Word8 -> Name -> Bool
contains word (Name name) =
  Text.isInfixOf (Text.singleton (Char.chr (fromIntegral word))) name


startsWith :: Name -> Name -> Bool
startsWith (Name prefix) (Name name) =
  Text.isPrefixOf prefix name


drop :: Int -> Name -> Name
drop numBytes (Name name) =
  Name (Text.drop numBytes name)



-- CONVERSIONS


toText :: Name -> Text.Text
toText (Name name) =
  name


toString :: Name -> String
toString (Name name) =
  Text.unpack name


toBuilder :: Name -> B.Builder
toBuilder (Name name) =
  Text.encodeUtf8Builder name


toDotlessBuilder :: Name -> B.Builder
toDotlessBuilder (Name name) =
  Text.encodeUtf8Builder (Text.replace "." "$" name)


toShort :: Name -> S.ShortByteString
toShort (Name name) =
  S.toShort (Text.encodeUtf8 name)


addIndex :: Name -> Int -> Name
addIndex (Name name) index =
  Name (Text.append name (Text.pack (show index)))


addSafeIndex :: Name -> Int -> Name
addSafeIndex (Name name) index =
  Name $ Text.append name $ Text.pack $
    if Char.isDigit (Text.last name) then
      '_' : show index
    else
      show index


toCompositeName :: [Name] -> Name
toCompositeName names =
  Name (Text.cons '$' (Text.intercalate "$" (map _name names)))


sepBy :: Word8 -> Name -> Name -> Name
sepBy sep (Name home) (Name name) =
  Name (Text.concat [ home, Text.singleton (Char.chr (fromIntegral sep)), name ])



-- CREATE NAMES


fromForeignPtr :: ForeignPtr Word8 -> Int -> Int -> Name
fromForeignPtr fptr offset len =
  Name (Text.decodeUtf8 (B.PS fptr offset len))


fromString :: String -> Name
fromString str =
  Name (Text.pack str)


{- Takes a letter from 0 to 25 -}
fromLetter :: Int -> Name
fromLetter letter =
  Name (Text.singleton (Char.chr (97 + letter)))


fromText :: Text.Text -> Name
fromText =
  Name



-- INSTANCES


instance String.IsString Name where
  fromString str =
    Name (Text.pack str)


instance Binary Name where
  put (Name name) = put name
  get = Name <$> get



-- COMMON NAMES


{-# NOINLINE int #-}
int :: Name
int = "Int"


{-# NOINLINE float #-}
float :: Name
float = "Float"


{-# NOINLINE bool #-}
bool :: Name
bool = "Bool"


{-# NOINLINE char #-}
char :: Name
char = "Char"


{-# NOINLINE string #-}
string :: Name
string = "String"


{-# NOINLINE maybe #-}
maybe :: Name
maybe = "Maybe"


{-# NOINLINE result #-}
result :: Name
result = "Result"


{-# NOINLINE list #-}
list :: Name
list = "List"


{-# NOINLINE array #-}
array :: Name
array = "Array"


{-# NOINLINE dict #-}
dict :: Name
dict = "Dict"


{-# NOINLINE tuple #-}
tuple :: Name
tuple = "Tuple"


{-# NOINLINE jsArray #-}
jsArray :: Name
jsArray = "JsArray"


{-# NOINLINE task #-}
task :: Name
task = "Task"


{-# NOINLINE router #-}
router :: Name
router = "Router"


{-# NOINLINE cmd #-}
cmd :: Name
cmd = "Cmd"


{-# NOINLINE sub #-}
sub :: Name
sub = "Sub"


{-# NOINLINE platform #-}
platform :: Name
platform = "Platform"


{-# NOINLINE virtualDom #-}
virtualDom :: Name
virtualDom = "VirtualDom"


{-# NOINLINE shader #-}
shader :: Name
shader = "Shader"


{-# NOINLINE debug #-}
debug :: Name
debug = "Debug"


{-# NOINLINE debugger #-}
debugger :: Name
debugger = "Debugger"


{-# NOINLINE bitwise #-}
bitwise :: Name
bitwise = "Bitwise"


{-# NOINLINE basics #-}
basics :: Name
basics = "Basics"


{-# NOINLINE utils #-}
utils :: Name
utils = "Utils"


{-# NOINLINE negate #-}
negate :: Name
negate = "negate"


{-# NOINLINE true #-}
true :: Name
true = "True"


{-# NOINLINE false #-}
false :: Name
false = "False"


{-# NOINLINE value #-}
value :: Name
value = "Value"


{-# NOINLINE node #-}
node :: Name
node = "Node"


{-# NOINLINE program #-}
program :: Name
program = "Program"


{-# NOINLINE main #-}
main :: Name
main = "main"


{-# NOINLINE dollar #-}
dollar :: Name
dollar = "$"


{-# NOINLINE identity #-}
identity :: Name
identity = "identity"


{-# NOINLINE replModule #-}
replModule :: Name
replModule = "Elm_Repl"
