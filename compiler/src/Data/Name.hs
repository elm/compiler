{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, MagicHash, OverloadedStrings, UnboxedTuples #-}
module Data.Name
  ( Name
  , length
  , toUtf8
  , toBuilder
  , toEscapedBuilder
  , fromByteString
  , fromString
  -- ???
  , toText
  , toString
  , getKernel
  , isKernel
  , hasDot
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


import Control.Exception (assert)
import Data.Binary (Binary(..), Get, getWord8, putWord8)
import Data.Binary.Put (putBuilder)
import Data.Binary.Get.Internal (readN)
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Builder.Internal as B
import qualified Data.List as List
import Data.String (IsString(..))
import qualified Data.Text as Text
import qualified Data.Utf8 as Utf8
import Foreign.ForeignPtr (ForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr, plusPtr)
import GHC.Exts
  ( Int(I#), Ptr(Ptr), Char(C#)
  , RealWorld
  , ByteArray#, MutableByteArray#
  , isTrue#
  , newByteArray#, unsafeFreezeByteArray#
  , sizeofByteArray#
  , writeCharArray#, writeWord8OffAddr#
  , copyAddrToByteArray#, copyByteArrayToAddr#
  )
import GHC.IO
import GHC.ST (ST(ST), runST)
import GHC.Prim
import GHC.Word (Word8(W8#))
import Prelude hiding (length, maybe, negate)



-- UTF-8


data Name =
  Name ByteArray#



-- LENGTH


length :: Name -> Int
length (Name ba#) =
  I# (sizeofByteArray# ba#)



-- EQUAL


instance Eq Name where
  (==) (Name ba1#) (Name ba2#) =
    let
      !len1# = sizeofByteArray# ba1#
      !len2# = sizeofByteArray# ba2#
    in
    isTrue# (len1# ==# len2#)
    &&
    isTrue# (0# ==# compareByteArrays# ba1# 0# ba2# 0# len1#)



-- COMPARE


-- TODO see if this can be written any faster
instance Ord Name where
  compare (Name ba1#) (Name ba2#) =
    let
      !len1# = sizeofByteArray# ba1#
      !len2# = sizeofByteArray# ba2#
      !len#  = if isTrue# (len1# <# len2#) then len1# else len2#
      !cmp#  = compareByteArrays# ba1# 0# ba2# 0# len#
    in
    case () of
      _ | isTrue# (cmp# <# 0#)     -> LT
        | isTrue# (cmp# ># 0#)     -> GT
        | isTrue# (len1# <# len2#) -> LT
        | isTrue# (len1# ># len2#) -> GT
        | True                     -> EQ



-- STRING OVERLOADING


instance IsString Name where
  fromString str =
    runST
    (
      do  mba <- newByteArray (List.length str)
          writeChars mba 0 str
          unsafeFreeze mba
    )


writeChars :: MBA s -> Int -> [Char] -> ST s ()
writeChars !mba !i chars =
  case chars of
    [] ->
      return ()

    c:cs ->
      do  writeChar8 mba i c
          writeChars mba (i+1) cs



-- BINARY


instance Binary Name where
  put name@(Name ba#) =
    putWord8 (W8# (int2Word# (sizeofByteArray# ba#)))
    >> putBuilder (toBuilder name)
  get =
    getWord8 >>= getName


{-# INLINE getName #-}
getName :: Word8 -> Get Name
getName w =
  let !n = fromIntegral w in
  readN n (copyFromByteString n)


{-# INLINE copyFromByteString #-}
copyFromByteString :: Int -> B.ByteString -> Name
copyFromByteString len (B.PS fptr offset _) =
  unsafeDupablePerformIO
  (
    do  mba <- stToIO (newByteArray len)
        stToIO (copyFromPtr (unsafeForeignPtrToPtr fptr `plusPtr` offset) mba 0 len)
        touchForeignPtr fptr
        stToIO (unsafeFreeze mba)
  )



-- FROM BYTESTRING


fromByteString :: ForeignPtr Word8 -> Int -> Int -> Name
fromByteString fptr offset len =
  unsafeDupablePerformIO
  (
    do  mba <- stToIO (newByteArray len)
        stToIO (copyFromPtr (unsafeForeignPtrToPtr fptr `plusPtr` offset) mba 0 len)
        touchForeignPtr fptr
        stToIO (unsafeFreeze mba)
  )



-- TO UTF-8


{-# INLINE toUtf8 #-}
toUtf8 :: Name -> Utf8.Utf8
toUtf8 (Name ba#) =
  Utf8.Utf8 ba#



-- TO BUILDER


{-# INLINE toBuilder #-}
toBuilder :: Name -> B.Builder
toBuilder =
  \name -> B.builder (toBuilderHelp name)


{-# INLINE toBuilderHelp #-}
toBuilderHelp :: Name -> B.BuildStep a -> B.BuildStep a
toBuilderHelp !name@(Name ba#) k =
    go 0 (I# (sizeofByteArray# ba#))
  where
    go !offset !end !(B.BufferRange bOffset bEnd) =
      let
        !bLen = minusPtr bEnd bOffset
        !len = end - offset
      in
      if len <= bLen then
        do  stToIO (copyToPtr name offset bOffset len)
            let !newBufferRange = B.BufferRange (plusPtr bOffset len) bEnd
            k newBufferRange
      else
        do  stToIO (copyToPtr name offset bOffset bLen)
            let !newOffset = offset + bLen
            return $ B.bufferFull 1 bEnd (go newOffset end)



-- TO ESCAPED BUILDER


{-# INLINE toEscapedBuilder #-}
toEscapedBuilder :: Word8 -> Word8 -> Name -> B.Builder
toEscapedBuilder before after =
  \name -> B.builder (toEscapedBuilderHelp before after name)


{-# INLINE toEscapedBuilderHelp #-}
toEscapedBuilderHelp :: Word8 -> Word8 -> Name -> B.BuildStep a -> B.BuildStep a
toEscapedBuilderHelp before after !name@(Name ba#) k =
    go 0 (I# (sizeofByteArray# ba#))
  where
    go !offset !len !(B.BufferRange bOffset bEnd) =
      let
        !bLen = minusPtr bEnd bOffset
      in
      if len <= bLen then
        do  -- TODO test if writing word-by-word is faster
            stToIO (copyToPtr name offset bOffset len)
            stToIO (escape before after bOffset name offset len)
            let !newBufferRange = B.BufferRange (plusPtr bOffset len) bEnd
            k newBufferRange
      else
        do  stToIO (copyToPtr name offset bOffset bLen)
            stToIO (escape before after bOffset name offset bLen)
            let !newOffset = offset + bLen
            let !newLength = len - bLen
            return $ B.bufferFull 1 bEnd (go newOffset newLength)


escape :: Word8 -> Word8 -> Ptr a -> Name -> Int -> Int -> ST RealWorld ()
escape before@(W8# before#) after ptr name@(Name ba#) offset@(I# offset#) end@(I# end#) =
  if isTrue# (offset# <# end#) then
    if isTrue# (eqWord# before# (indexWord8Array# ba# offset#))
    then
      do  writeWordToPtr ptr offset after
          escape before after ptr name (offset + 1) end
    else
      do  escape before after ptr name (offset + 1) end

  else
    return ()



-- TO STRING


toString :: Name -> String
toString name =
  Utf8.toString (toUtf8 name)



-- HAS DOT


hasDot :: Name -> Bool
hasDot (Name ba#) =
  let
    !len# = sizeofByteArray# ba#
  in
  hasDotHelp ba# 0# len#


hasDotHelp :: ByteArray# -> Int# -> Int# -> Bool
hasDotHelp ba# !offset# end# =
  if isTrue# (offset# <# end#) then
    if isTrue# (eqWord# 0x2E## {- . -} (indexWord8Array# ba# offset#))
      then True
      else hasDotHelp ba# (offset# +# 1#) end#

  else
    False



-- TO TEXT


toText :: Name -> Text.Text
toText name =
  Text.pack (toString name) -- TODO get rid of this?



-- GET KERNEL


getKernel :: Name -> Name
getKernel name@(Name ba#) =
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
                  (# s, ba# #) -> (# s, Name ba# #)
    )
  )



-- STARTS WITH


{-# INLINE startsWith #-}
startsWith :: Name -> Name -> Bool
startsWith (Name ba1#) (Name ba2#) =
  let
    !len1# = sizeofByteArray# ba1#
    !len2# = sizeofByteArray# ba2#
  in
  isTrue# (len1# <=# len2#)
  &&
  isTrue# (0# ==# compareByteArrays# ba1# 0# ba2# 0# len1#)


isKernel :: Name -> Bool
isKernel = startsWith prefix_kernel

isNumberType :: Name -> Bool
isNumberType = startsWith prefix_number

isComparableType :: Name -> Bool
isComparableType = startsWith prefix_comparable

isAppendableType :: Name -> Bool
isAppendableType = startsWith prefix_appendable

isCompappendType :: Name -> Bool
isCompappendType = startsWith prefix_compappend

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



-- FROM INDEX


fromVarIndex :: Int -> Name
fromVarIndex n =
  runST
  (
    do  let !size = getIndexSize n
        mba <- newByteArray size
        writeWord8 mba 0 0x5F {- _ -}
        writeWord8 mba 1 0x76 {- v -}
        writeNumbers mba (size - 1) n
        unsafeFreeze mba
  )


{-# INLINE getIndexSize #-}
getIndexSize :: Int -> Int
getIndexSize n
  | n < 10  = 3
  | n < 100 = 4
  | True    = 2 + ceiling (logBase 10 (fromIntegral n + 1) :: Float)



writeNumbers :: MBA s -> Int -> Int -> ST s ()
writeNumbers !mba !offset !n =
  do  let (q,r) = quotRem n 10
      writeWord8 mba offset (0x30 + fromIntegral r)
      if q <= 0
        then return ()
        else writeNumbers mba (offset-1) q


fromTypeVariable :: Name -> Int -> Name
fromTypeVariable name@(Name ba#) index =
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
        do  let !size = length name + 1 + getIndexSize index
            mba <- newByteArray size
            copyToMBA name mba
            writeWord8 mba (I# len#) 0x5F {- _ -}
            writeNumbers mba (size - 1) index
            unsafeFreeze mba
      )
    else
      runST
      (
        do  let !size = length name + getIndexSize index
            mba <- newByteArray size
            copyToMBA name mba
            writeNumbers mba (size - 1) index
            unsafeFreeze mba
      )


fromTypeVariableScheme :: Int -> Name
fromTypeVariableScheme scheme =
  runST
  (
    if scheme < 26 then
      do  mba <- newByteArray 1
          writeWord8 mba 0 (0x61 + fromIntegral scheme)
          unsafeFreeze mba
    else
      do  let (extra, letter) = quotRem scheme 26
          let !size = getIndexSize extra
          mba <- newByteArray size
          writeWord8 mba 0 (0x61 + fromIntegral letter)
          writeNumbers mba (size - 1) extra
          unsafeFreeze mba
  )


fromReplCount :: Int -> Name
fromReplCount n =
  fromString ("repl_value_" ++ show n)



-- FROM MANY NAMES


fromManyNames :: [Name] -> Name
fromManyNames names =
  let
    !(I# size#) = sum (map (\(Name ba#) -> I# (sizeofByteArray# ba# +# 1#)) names)
  in
  runST
  (
    ST $ \s ->
      case newByteArray# size# s of
        (# s, mba# #) ->
          case writeNames mba# 0# names s of
            s ->
              case unsafeFreezeByteArray# mba# s of
                (# s, ba# #) -> (# s, Name ba# #)
  )


writeNames :: MutableByteArray# s -> Int# -> [Name] -> State# s -> State# s
writeNames mba# !offset# names s =
  case names of
    [] ->
      s

    (Name ba#) : names ->
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
        unsafeFreeze mba
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
sepBy (W8# sep#) (Name ba1#) (Name ba2#) =
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
                        (# s, ba# #) -> (# s, Name ba# #)
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


{-# INLINE unsafeFreeze #-}
unsafeFreeze :: MBA s -> ST s Name
unsafeFreeze (MBA# mba#) =
  ST $ \s ->
    case unsafeFreezeByteArray# mba# s of
      (# s, ba# #) -> (# s, Name ba# #)


{-# INLINE writeChar8 #-}
writeChar8 :: MBA s -> Int -> Char -> ST s ()
writeChar8 (MBA# mba#) (I# offset#) (C# char#) =
  ST $ \s ->
    case writeCharArray# mba# offset# char# s of
      s -> (# s, () #)


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


{-# INLINE copyFromPtr #-}
copyFromPtr :: Ptr a -> MBA RealWorld -> Int -> Int -> ST RealWorld ()
copyFromPtr (Ptr addr#) (MBA# mba#) (I# offset#) (I# len#) =
  ST $ \s ->
    case copyAddrToByteArray# addr# mba# offset# len# s of
      s -> (# s, () #)


{-# INLINE copyToPtr #-}
copyToPtr :: Name -> Int -> Ptr a -> Int -> ST RealWorld ()
copyToPtr (Name ba#) (I# offset#) (Ptr addr#) (I# len#) =
    ST $ \s ->
      case copyByteArrayToAddr# ba# offset# addr# len# s of
        s -> (# s, () #)


{-# INLINE copyToMBA #-}
copyToMBA :: Name -> MBA s -> ST s ()
copyToMBA (Name ba#) (MBA# mba#) =
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
