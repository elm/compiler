{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Name
  ( Name
  , length
  , toString
  , toBuilder
  , toShort
  , localFromInt
  , toCompositeName
  , int, float, bool, char, string
  , maybe, list, array, tuple
  , task, router, cmd, sub, platform
  , shader, debug, bitwise, basics
  , utils, negate, value
  , node, program, main, dollar
  )
  where


import Prelude hiding (length, maybe, negate)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Short as S
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text



-- NAME


type Name = Text.Text


length :: Name -> Int
length =
  Text.length


toString :: Name -> String
toString =
  Text.unpack


toBuilder :: Name -> B.Builder
toBuilder =
  Text.encodeUtf8Builder


toShort :: Name -> S.ShortByteString
toShort =
  error "TODO"


localFromInt :: Int -> Name
localFromInt n =
  Text.pack ('x' : show n)


toCompositeName :: Set.Set Name -> Name
toCompositeName names =
  Text.cons '$' (Text.intercalate "$" (Set.toList names))



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
