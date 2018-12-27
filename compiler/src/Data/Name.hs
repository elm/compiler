{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, EmptyDataDecls, MagicHash, OverloadedStrings, UnboxedTuples #-}
module Data.Name
  ( Name
  , toUtf8
  , fromUtf8
  , toEscapedBuilder
  , Utf8.toBuilder
  , Utf8.fromPtr
  , Utf8.fromChars
  , Utf8.toChars
  -- misc
  , getKernel
  , hasDot
  , isKernel
  , isNumberType
  , isComparableType
  , isAppendableType
  , isCompappendType
  , fromVarIndex
  , fromReplCount
  , fromWords
  , fromManyNames
  , fromTypeVariable
  , fromTypeVariableScheme
  , sepBy
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


import Prelude hiding (length, maybe, negate)
import Control.Exception (assert)
import qualified Data.ByteString.Builder.Internal as B
import Data.Coerce (coerce)
import qualified Data.List as List
import qualified Data.Utf8 as Utf8
import Foreign.Ptr (plusPtr, minusPtr)
import GHC.Exts
  ( Int(I#), Ptr(Ptr)
  , RealWorld
  , MutableByteArray#
  , isTrue#
  , newByteArray#
  , sizeofByteArray#
  , writeWord8OffAddr#
  , copyByteArrayToAddr#
  , unsafeFreezeByteArray#
  )
import GHC.IO
import GHC.ST (ST(ST), runST)
import GHC.Prim
import GHC.Word (Word8(W8#))



-- NAME


type Name = Utf8.Under256 NAME


data NAME


toUtf8 :: Name -> Utf8.String
toUtf8 =
  coerce


fromUtf8 :: Utf8.String -> Name
fromUtf8 =
  coerce



-- TO ESCAPED BUILDER


{-# INLINE toEscapedBuilder #-}
toEscapedBuilder :: Word8 -> Word8 -> Name -> B.Builder
toEscapedBuilder before after =
  \name -> B.builder (toEscapedBuilderHelp before after name)


{-# INLINE toEscapedBuilderHelp #-}
toEscapedBuilderHelp :: Word8 -> Word8 -> Name -> B.BuildStep a -> B.BuildStep a
toEscapedBuilderHelp before after !name@(Utf8.Utf8 ba#) k =
    go 0 (I# (sizeofByteArray# ba#))
  where
    go !offset !len !(B.BufferRange bOffset bEnd) =
      let
        !bLen = minusPtr bEnd bOffset
      in
      if len <= bLen then
        do  -- TODO test if writing word-by-word is faster
            stToIO (copyToPtr name offset bOffset len)
            stToIO (escape before after bOffset name offset len 0)
            let !newBufferRange = B.BufferRange (plusPtr bOffset len) bEnd
            k newBufferRange
      else
        do  stToIO (copyToPtr name offset bOffset bLen)
            stToIO (escape before after bOffset name offset bLen 0)
            let !newOffset = offset + bLen
            let !newLength = len - bLen
            return $ B.bufferFull 1 bEnd (go newOffset newLength)


escape :: Word8 -> Word8 -> Ptr a -> Name -> Int -> Int -> Int -> ST RealWorld ()
escape before@(W8# before#) after ptr name@(Utf8.Utf8 ba#) offset@(I# offset#) len@(I# len#) i@(I# i#) =
  if isTrue# (i# <# len#) then
    if isTrue# (eqWord# before# (indexWord8Array# ba# (offset# +# i#)))
    then
      do  writeWordToPtr ptr i after
          escape before after ptr name offset len (i + 1)
    else
      do  escape before after ptr name offset len (i + 1)

  else
    return ()



-- HAS DOT


hasDot :: Name -> Bool
hasDot name =
  Utf8.contains 0x2E {- . -} (coerce name)



-- GET KERNEL


getKernel :: Name -> Name
getKernel name@(Utf8.Utf8 ba#) =
  assert (isKernel name)
  (
    runST
    (
      let
        !size# = sizeofByteArray# ba# -# 11#
      in
      ST $ \s ->
        case newByteArray# size# s of
          (# s, mba# #) ->
            case copyByteArray# ba# 11# mba# 0# size# s of
              s ->
                case unsafeFreezeByteArray# mba# s of
                  (# s, ba# #) -> (# s, Utf8.Utf8 ba# #)
    )
  )



-- STARTS WITH


isKernel :: Name -> Bool
isKernel = Utf8.startsWith prefix_kernel

isNumberType :: Name -> Bool
isNumberType = Utf8.startsWith prefix_number

isComparableType :: Name -> Bool
isComparableType = Utf8.startsWith prefix_comparable

isAppendableType :: Name -> Bool
isAppendableType = Utf8.startsWith prefix_appendable

isCompappendType :: Name -> Bool
isCompappendType = Utf8.startsWith prefix_compappend

{-# NOINLINE prefix_kernel #-}
prefix_kernel :: Name
prefix_kernel = "Elm.Kernel."

{-# NOINLINE prefix_number #-}
prefix_number :: Name
prefix_number = "number"

{-# NOINLINE prefix_comparable #-}
prefix_comparable :: Name
prefix_comparable = "comparable"

{-# NOINLINE prefix_appendable #-}
prefix_appendable :: Name
prefix_appendable = "appendable"

{-# NOINLINE prefix_compappend #-}
prefix_compappend :: Name
prefix_compappend = "compappend"



-- FROM VAR INDEX


fromVarIndex :: Int -> Name
fromVarIndex n =
  runST
  (
    do  let !size = getIndexSize n
        mba <- newByteArray size
        writeWord8 mba 0 0x5F {- _ -}
        writeWord8 mba 1 0x76 {- v -}
        writeDigits mba (size - 1) n
        freeze mba
  )


{-# INLINE getIndexSize #-}
getIndexSize :: Int -> Int
getIndexSize n
  | n < 10  = 3
  | n < 100 = 4
  | True    = 2 + ceiling (logBase 10 (fromIntegral n + 1) :: Float)



writeDigits :: MBA s -> Int -> Int -> ST s ()
writeDigits !mba !offset !n =
  do  let (q,r) = quotRem n 10
      writeWord8 mba offset (0x30 + fromIntegral r)
      if q <= 0
        then return ()
        else writeDigits mba (offset-1) q



-- FROM TYPE VARIABLE


fromTypeVariable :: Name -> Int -> Name
fromTypeVariable name@(Utf8.Utf8 ba#) index =
  if index <= 0 then
    name
  else
    let
      len# = sizeofByteArray# ba#
      end# = indexWord8Array# ba# (len# -# 1#)
    in
    if isTrue# (leWord# 0x30## end#) && isTrue# (leWord# end# 0x39##) then
      runST
      (
        do  let !size = I# len# + 1 + getIndexSize index
            mba <- newByteArray size
            copyToMBA name mba
            writeWord8 mba (I# len#) 0x5F {- _ -}
            writeDigits mba (size - 1) index
            freeze mba
      )
    else
      runST
      (
        do  let !size = I# len# + getIndexSize index
            mba <- newByteArray size
            copyToMBA name mba
            writeDigits mba (size - 1) index
            freeze mba
      )



-- FROM TYPE VARIABLE SCHEME


fromTypeVariableScheme :: Int -> Name
fromTypeVariableScheme scheme =
  runST
  (
    if scheme < 26 then
      do  mba <- newByteArray 1
          writeWord8 mba 0 (0x61 + fromIntegral scheme)
          freeze mba
    else
      do  let (extra, letter) = quotRem scheme 26
          let !size = getIndexSize extra
          mba <- newByteArray size
          writeWord8 mba 0 (0x61 + fromIntegral letter)
          writeDigits mba (size - 1) extra
          freeze mba
  )



-- FROM REPL COUNT


fromReplCount :: Int -> Name
fromReplCount n =
  Utf8.fromChars ("repl_value_" ++ show n)



-- FROM MANY NAMES


fromManyNames :: [Name] -> Name
fromManyNames names =
  let
    !(I# size#) = sum (map (\(Utf8.Utf8 ba#) -> I# (sizeofByteArray# ba# +# 1#)) names)
  in
  runST
  (
    ST $ \s ->
      case newByteArray# size# s of
        (# s, mba# #) ->
          case writeNames mba# 0# names s of
            s ->
              case unsafeFreezeByteArray# mba# s of
                (# s, ba# #) -> (# s, Utf8.Utf8 ba# #)
  )


writeNames :: MutableByteArray# s -> Int# -> [Name] -> State# s -> State# s
writeNames mba# !offset# names s =
  case names of
    [] ->
      s

    (Utf8.Utf8 ba#) : names ->
      case writeWord8Array# mba# offset# 0x24## {- $ -} s of
        s ->
          let
            !offset1# = offset# +# 1#
            !len# = sizeofByteArray# ba#
            !newOffset# = offset1# +# len#
          in
          case copyByteArray# ba# 0# mba# offset1# len# s of
            s ->
              writeNames mba# newOffset# names s



-- FROM WORDS


fromWords :: [Word8] -> Name
fromWords words =
  runST
  (
    do  mba <- newByteArray (List.length words)
        writeWords mba 0 words
        freeze mba
  )


writeWords :: MBA s -> Int -> [Word8] -> ST s ()
writeWords !mba !i words =
  case words of
    [] ->
      return ()

    w:ws ->
      do  writeWord8 mba i w
          writeWords mba (i+1) ws



-- SEP BY


sepBy :: Word8 -> Name -> Name -> Name
sepBy (W8# sep#) (Utf8.Utf8 ba1#) (Utf8.Utf8 ba2#) =
  let
    !len1# = sizeofByteArray# ba1#
    !len2# = sizeofByteArray# ba2#
  in
  runST
  (
    ST $ \s ->
      case newByteArray# (len1# +# len2# +# 1#) s of
        (# s, mba# #) ->
          case copyByteArray# ba1# 0# mba# 0# len1# s of
            s ->
              case writeWord8Array# mba# len1# sep# s of
                s ->
                  case copyByteArray# ba2# 0# mba# (len1# +# 1#) len2# s of
                    s ->
                      case unsafeFreezeByteArray# mba# s of
                        (# s, ba# #) -> (# s, Utf8.Utf8 ba# #)
  )



-- PRIMITIVES


data MBA s =
  MBA# (MutableByteArray# s)


{-# INLINE newByteArray #-}
newByteArray :: Int -> ST s (MBA s)
newByteArray (I# len#) =
  ST $ \s ->
    case newByteArray# len# s of
      (# s, mba# #) -> (# s, MBA# mba# #)


{-# INLINE freeze #-}
freeze :: MBA s -> ST s Name
freeze (MBA# mba#) =
  ST $ \s ->
    case unsafeFreezeByteArray# mba# s of
      (# s, ba# #) -> (# s, Utf8.Utf8 ba# #)


{-# INLINE writeWord8 #-}
writeWord8 :: MBA s -> Int -> Word8 -> ST s ()
writeWord8 (MBA# mba#) (I# offset#) (W8# w#) =
  ST $ \s ->
    case writeWord8Array# mba# offset# w# s of
      s -> (# s, () #)


{-# INLINE writeWordToPtr #-}
writeWordToPtr :: Ptr a -> Int -> Word8 -> ST s ()
writeWordToPtr (Ptr addr#) (I# offset#) (W8# word#) =
  ST $ \s ->
    case writeWord8OffAddr# addr# offset# word# s of
      s -> (# s, () #)


{-# INLINE copyToPtr #-}
copyToPtr :: Name -> Int -> Ptr a -> Int -> ST RealWorld ()
copyToPtr (Utf8.Utf8 ba#) (I# offset#) (Ptr addr#) (I# len#) =
    ST $ \s ->
      case copyByteArrayToAddr# ba# offset# addr# len# s of
        s -> (# s, () #)


{-# INLINE copyToMBA #-}
copyToMBA :: Name -> MBA s -> ST s ()
copyToMBA (Utf8.Utf8 ba#) (MBA# mba#) =
    ST $ \s ->
      case copyByteArray# ba# 0# mba# 0# (sizeofByteArray# ba#) s of
        s -> (# s, () #)



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
