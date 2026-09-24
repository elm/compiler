{-# LANGUAGE BangPatterns, CPP, ExtendedLiterals, MagicHash, QuasiQuotes, UnboxedTuples #-}
module AST.Prim.Module
  ( Name
  , size
  , toChars
  , toString
  , toBuilder
  , toFilePath
  , toDashPath
  , toPrefix
  --
  , fromAddr
  , fromString
  --
  , isKernel
  , getKernel
  --
  , Name_LEXICAL
  , toLexical
  , fromLexical
  --
  , Prefix
  , prefixToChars
  , prefixToString
  , prefixFromAddr
  --
  , Kernel
  , kernelToString
  , kernelToBuilder
  --
  , main
  , repl
  --
  , basics, bitwise, debug, list, array, dict
  , maybe, result, string, char, tuple
  , platform, platform_cmd, platform_sub
  , prefix_cmd, prefix_sub
  , kernel_debug, kernel_debugger, kernel_list
  , kernel_utils, kernel_platform, kernel_vdom
  , jsArray, virtualDom
  , json_decode, json_encode
  , webgl, webgl_texture
  , math_vector2, math_vector3, math_vector4, math_matrix4
  --
  , eName, dName
  , ePrefix, dPrefix
  , eKernel, dKernel
  --
  , jsonEncodeName
  , jsonDecodeName
  --
  , Problem(..)
  , chomper
  )
  where


import Prelude hiding (maybe, sequence)
import qualified Data.ByteString.Builder as B
import qualified Data.List as List
import GHC.Exts (isTrue#)
import GHC.Prim
import GHC.ST (ST(ST), runST)
import qualified System.FilePath as FP

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Json.Decode as JD
import qualified Json.Encode as JE
import qualified String as S
import String (ascii)
import qualified ThreadSafe.Fork as Fork

import qualified AST.Prim.Variable as Var
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A



-- NAME


newtype Name =
  Name S.String


size :: Name -> Int
size (Name s) =
  S.size s


toChars :: Name -> [Char]
toChars (Name s) =
  S.toChars s


toString :: Name -> S.String
toString (Name s) =
  s


toBuilder :: Name -> B.Builder
toBuilder (Name s) =
  S.toBuilder s


toFilePath :: Name -> FilePath
toFilePath name =
  List.map (\c -> if c == '.' then FP.pathSeparator else c) (toChars name)


toDashPath :: Name -> FilePath
toDashPath name =
  List.map (\c -> if c == '.' then '-' else c) (toChars name)


toPrefix :: Name -> Prefix
toPrefix (Name s) =
  Prefix s


fromAddr :: Addr# -> Addr# -> IO Name
fromAddr pos end =
  Name <$> S.fromAddr pos end


fromString :: S.String -> Name
fromString =
  Name


instance Eq  Name where (==)    (Name s1) (Name s2) = S.equal       s1 s2
instance Ord Name where compare (Name s1) (Name s2) = S.compareFast s1 s2

instance Fork.Context Name where
  toContextChars name = "in module " ++ toChars name



-- KERNEL


isKernel :: Name -> Bool
isKernel (Name (S.String ba)) =
    isTrue# (compareByteArrays# ba 0# prefix 0# (sizeofByteArray# prefix) ==# 0#)
  where
    !(S.String prefix) = kernel_prefix


{-# NOINLINE kernel_prefix #-}
kernel_prefix :: S.String
kernel_prefix =
  [S.ascii|Elm.Kernel.|]


getKernel :: Name -> Kernel
getKernel (Name (S.String ba)) =
  runST $ ST $ \s0 ->
    case newByteArray# len                 s0 of { (# s1, mba #) ->
    case copyByteArray# ba plen mba 0# len s1 of {    s2         ->
    case unsafeFreezeByteArray# mba        s2 of { (# s3, ba' #) ->
      (# s3, Kernel (S.String ba') #)
    }}}
  where
    !(S.String prefix) = kernel_prefix
    plen = sizeofByteArray# prefix
    len = sizeofByteArray# ba -# plen



-- LEXICAL


newtype Name_LEXICAL =
  Name_LEXICAL S.String

instance Eq  Name_LEXICAL where (==)    (Name_LEXICAL s1) (Name_LEXICAL s2) = S.equal          s1 s2
instance Ord Name_LEXICAL where compare (Name_LEXICAL s1) (Name_LEXICAL s2) = S.compareLexical s1 s2


toLexical :: Name -> Name_LEXICAL
toLexical =
  coerce


fromLexical :: Name_LEXICAL -> Name
fromLexical =
  coerce



-- PREFIX


newtype Prefix =
  Prefix S.String


prefixToChars :: Prefix -> [Char]
prefixToChars (Prefix s) =
  S.toChars s


prefixToString :: Prefix -> S.String
prefixToString (Prefix s) =
  s


prefixFromAddr :: Addr# -> Addr# -> IO Prefix
prefixFromAddr pos end =
  Prefix <$> S.fromAddr pos end


instance Eq Prefix where
  (==) (Prefix s1) (Prefix s2) =
    S.equal s1 s2

instance Ord Prefix where
  compare (Prefix s1) (Prefix s2) =
    S.compareFast s1 s2



-- KERNEL


newtype Kernel =
  Kernel S.String


kernelToString :: Kernel -> S.String
kernelToString =
  coerce


kernelToBuilder :: Kernel -> B.Builder
kernelToBuilder k =
  S.toBuilder (coerce k)



-- DEFAULT MODULE NAMES


main :: Name; main = Name [ascii|Main|]
repl :: Name; repl = Name [ascii|Elm_Repl|]



-- CORE MODULE NAMES


basics       :: Name; basics       = Name [ascii|Basics|]
bitwise      :: Name; bitwise      = Name [ascii|Bitwise|]
debug        :: Name; debug        = Name [ascii|Debug|]
list         :: Name; list         = Name [ascii|List|]
array        :: Name; array        = Name [ascii|Array|]
dict         :: Name; dict         = Name [ascii|Dict|]
maybe        :: Name; maybe        = Name [ascii|Maybe|]
result       :: Name; result       = Name [ascii|Result|]
string       :: Name; string       = Name [ascii|String|]
char         :: Name; char         = Name [ascii|Char|]
tuple        :: Name; tuple        = Name [ascii|Tuple|]
platform     :: Name; platform     = Name [ascii|Platform|]
platform_cmd :: Name; platform_cmd = Name [ascii|Platform.Cmd|]
platform_sub :: Name; platform_sub = Name [ascii|Platform.Sub|]

prefix_cmd   :: Prefix; prefix_cmd   = Prefix [ascii|Cmd|]
prefix_sub   :: Prefix; prefix_sub   = Prefix [ascii|Sub|]

kernel_debug    :: Kernel; kernel_debug    = Kernel [ascii|Debug|]
kernel_debugger :: Kernel; kernel_debugger = Kernel [ascii|Debugger|]
kernel_list     :: Kernel; kernel_list     = Kernel [ascii|List|]
kernel_utils    :: Kernel; kernel_utils    = Kernel [ascii|Utils|]
kernel_platform :: Kernel; kernel_platform = Kernel [ascii|Platform|]
kernel_vdom     :: Kernel; kernel_vdom     = Kernel [ascii|VirtualDom|]

jsArray       :: Name; jsArray       = Name [ascii|JsArray|]
virtualDom    :: Name; virtualDom    = Name [ascii|VirtualDom|]
json_decode   :: Name; json_decode   = Name [ascii|Json.Decode|]
json_encode   :: Name; json_encode   = Name [ascii|Json.Encode|]
webgl         :: Name; webgl         = Name [ascii|WebGL|]
webgl_texture :: Name; webgl_texture = Name [ascii|WebGL.Texture|]
math_vector2  :: Name; math_vector2  = Name [ascii|Math.Vector2|]
math_vector3  :: Name; math_vector3  = Name [ascii|Math.Vector3|]
math_vector4  :: Name; math_vector4  = Name [ascii|Math.Vector4|]
math_matrix4  :: Name; math_matrix4  = Name [ascii|Math.Matrix4|]



-- BYTES ENCODE AND DECODE


eName   :: Name   -> E.Builder; eName   n = E.string8 (coerce n)
ePrefix :: Prefix -> E.Builder; ePrefix n = E.string8 (coerce n)
eKernel :: Kernel -> E.Builder; eKernel n = E.string8 (coerce n)

dName   :: D.Decoder Name  ; dName   = coerce D.string8
dPrefix :: D.Decoder Prefix; dPrefix = coerce D.string8
dKernel :: D.Decoder Kernel; dKernel = coerce D.string8



-- JSON


jsonEncodeName :: Name -> JE.Value
jsonEncodeName (Name str) =
  JE.string str


jsonDecodeName :: (A.Position -> x) -> JD.Decoder x Name
jsonDecodeName toErr =
    JD.customString parser err
  where
    err cur =
      toErr (A.Position cur)

    parser =
      P.Parser $ \_ (P.State pos end indent cur) cok _ cerr eerr ->
        chomper pos end
          (\p _ -> (if P.eqAddr pos p then eerr else cerr) (slide cur pos p) err)
          (\p n -> cok n (P.State p end indent (slide cur pos p)))

    slide cur start end =
      P.slide cur (wordToWord64# (int2Word# (minusAddr# end start)))



-- CHOMPER


data Problem
  = NeedUpperAtStart
  | NeedUpperAfterDot
  | Over256
  | Stuck


chomper :: Addr# -> Addr# -> (Addr# -> Problem -> IO r) -> (Addr# -> Name -> IO r) -> IO r
chomper start end err ok =
    first start NeedUpperAtStart
  where
    first pos problem =
      if isTrue# (ltAddr# pos end)
      then
        let
          !next = Var.chompUpper pos end (indexWord8OffAddr# pos 0#)
        in
        if isTrue# (eqAddr# pos next)
          then err pos problem
          else inner next
      else
        err pos problem

    inner pos =
      if isTrue# (ltAddr# pos end)
      then
        let
          !w = indexWord8OffAddr# pos 0#
        in
        case Var.chompInner pos end w of
          !next
            | isTrue# (ltAddr# pos next)            -> inner next
            | isTrue# (eqWord8# w 0x2E#Word8 {-.-}) -> first (plusAddr# pos 1#) NeedUpperAfterDot
            | otherwise                             -> done pos
      else
        done pos

    done pos =
      if isTrue# (minusAddr# end pos <# 256#)
      then ok pos . Name =<< S.fromAddr start pos
      else err pos Over256

