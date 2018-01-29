{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Name
  ( Name
  , length
  , startsWith
  , drop
  , toString
  , toBuilder
  , toShort
  , addIndex
  , addSafeIndex
  , toCompositeName
  , fromForeignPtr
  , int, float, bool, char, string
  , maybe, list, array, tuple, jsArray
  , task, router, cmd, sub, platform, browser
  , shader, debug, bitwise, basics
  , utils, negate, value
  , node, program, main, dollar, identity
  )
  where


import Prelude hiding (drop, length, maybe, negate)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Short as S
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)



-- NAME


type Name = Text.Text


length :: Name -> Int
length =
  Text.length


startsWith :: Name -> Name -> Bool
startsWith =
  Text.isPrefixOf


drop :: Int -> Name -> Name
drop =
  Text.drop


toString :: Name -> String
toString =
  Text.unpack


toBuilder :: Name -> B.Builder
toBuilder =
  Text.encodeUtf8Builder


toShort :: Name -> S.ShortByteString
toShort name =
  error "TODO" name


addIndex :: Name -> Int -> Name
addIndex name index =
  Text.append name (Text.pack (show index))


addSafeIndex :: Name -> Int -> Name
addSafeIndex name index =
  Text.append name $ Text.pack $
    if Char.isDigit (Text.last name) then
      '_' : show index
    else
      show index


toCompositeName :: Set.Set Name -> Name
toCompositeName names =
  Text.cons '$' (Text.intercalate "$" (Set.toList names))



-- FROM PARSER


fromForeignPtr :: ForeignPtr Word8 -> Int -> Int -> Name
fromForeignPtr fptr offset len =
  Text.decodeUtf8 (B.PS fptr offset len)



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


{-# NOINLINE list #-}
list :: Name
list = "List"


{-# NOINLINE array #-}
array :: Name
array = "Array"


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


{-# NOINLINE browser #-}
browser :: Name
browser = "Browser"


{-# NOINLINE shader #-}
shader :: Name
shader = "Shader"


{-# NOINLINE debug #-}
debug :: Name
debug = "Debug"


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
