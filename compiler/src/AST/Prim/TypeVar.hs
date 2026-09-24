{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, PatternSynonyms,
QuasiQuotes, UnliftedNewtypes, UnboxedTuples
#-}
module AST.Prim.TypeVar
  ( Var(..)
  , Constraint(Any, Comparable, Appendable, CompAppend, Number)
  , varFromAddr
  , varFromString
  , varToChars
  , varToString
  --
  , unifyFlexWithFlex
  , unifyRigidWithFlex
  --
  , a, b, c
  --
  , genAny
  , genComparable
  , genAppendable
  , genCompAppend
  , genNumber
  --
  , genIndexed
  --
  , eVar
  , dVar
  )
  where


import GHC.Exts (isTrue#)
import GHC.Int (Int(..))
import GHC.Prim
import GHC.ST (ST(ST), runST)

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Literals
import qualified String as S



-- TYPE VARIABLE
--
-- Type variables have constraints that only unify with a limited set of types.
-- They are useful for some common infix operators:
--
--   (<)  : comparable -> comparable -> Bool
--   (+)  : number -> number -> number
--   (*)  : number -> number -> number
--


data Var = Var S.String Constraint


instance Eq Var where
  (==) (Var x (Constraint x')) (Var y (Constraint y')) =
    isTrue# (eqWord8# x' y') && x == y

instance Ord Var where
  compare (Var x (Constraint x')) (Var y (Constraint y'))
    | isTrue# (eqWord8# x' y') = compare x y
    | isTrue# (ltWord8# x' y') = LT
    | otherwise                = GT



-- TYPE VARIABLE CONSTRAINT
--
-- Connstraints are represented as numbers so that they can be unified quickly
-- using a lookup table.


newtype Constraint = Constraint Word8#

{-# INLINE Any        #-}
{-# INLINE Comparable #-}
{-# INLINE Appendable #-}
{-# INLINE CompAppend #-}
{-# INLINE Number     #-}

{-# COMPLETE Any, Comparable, Appendable, CompAppend, Number :: Constraint #-}
pattern Any, Comparable, Appendable, CompAppend, Number :: Constraint

pattern Any        = Constraint 0#Word8
pattern Comparable = Constraint 1#Word8
pattern Appendable = Constraint 2#Word8
pattern CompAppend = Constraint 3#Word8
pattern Number     = Constraint 4#Word8




-- UNIFY FLEX WITH FLEX


{-# INLINE unifyFlexWithFlex #-}
unifyFlexWithFlex :: Constraint -> Constraint -> a -> a -> a -> a -> a -> a
unifyFlexWithFlex (Constraint x) (Constraint y) unifyEqual unifyLeft unifyRight unifyCompAppend unifyError =
  case Literals.read tbl (uncheckedShiftLWord8# x 4# `orWord8#` y) of
    0#Word8 -> unifyEqual
    1#Word8 -> unifyLeft
    2#Word8 -> unifyRight
    3#Word8 -> unifyCompAppend
    _       -> unifyError
  where
    !(Literals.Table tbl) = flexWithFlex


flexWithFlex :: Literals.Table
flexWithFlex =
  [Literals.table|
    0 2 2 2 2 _ _ _ _ _ _ _ _ _ _ _
    1 0 3 2 2 _ _ _ _ _ _ _ _ _ _ _
    1 3 0 2 _ _ _ _ _ _ _ _ _ _ _ _
    1 1 1 0 _ _ _ _ _ _ _ _ _ _ _ _
    1 1 _ _ 0 _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  |]



-- UNIFY RIGID WITH FLEX


{-# INLINE unifyRigidWithFlex #-}
unifyRigidWithFlex :: Constraint -> Constraint -> a -> a -> a
unifyRigidWithFlex (Constraint x) (Constraint y) ok err =
  case Literals.read tbl (uncheckedShiftLWord8# x 4# `orWord8#` y) of
    0#Word8 -> ok
    _       -> err
  where
    !(Literals.Table tbl) = rigidWithFlex


rigidWithFlex :: Literals.Table
rigidWithFlex =
  [Literals.table|
    0 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    0 0 _ _ _ _ _ _ _ _ _ _ _ _ _ _
    0 _ 0 _ _ _ _ _ _ _ _ _ _ _ _ _
    0 0 0 0 _ _ _ _ _ _ _ _ _ _ _ _
    0 0 _ _ 0 _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
  |]



-- TO CHARS


varToChars :: Var -> [Char]
varToChars (Var name _) =
  S.toChars name


varToString :: Var -> S.String
varToString (Var name _) =
  name



-- FROM SOURCE
--
-- When reading a variable from a source file, it may be either a Var or a Con
-- depending on the particular name. It will get turned into an Ext in a later
-- phase depending on the usage.


{-# INLINE varFromAddr #-}
varFromAddr :: Addr# -> Addr# -> IO Var
varFromAddr pos end =
  varFromString <$> S.fromAddr pos end


varFromString :: S.String -> Var
varFromString s@(S.String ba)
  | isTrue# (len <# 6#)          = Var s Any
  | startsWith comparable ba len = Var s Comparable
  | startsWith appendable ba len = Var s Appendable
  | startsWith compappend ba len = Var s CompAppend
  | startsWith number     ba len = Var s Number
  | otherwise                    = Var s Any
  where
    !len = sizeofByteArray# ba


{-# INLINE startsWith #-}
startsWith :: S.String -> ByteArray# -> Int# -> Bool
startsWith (S.String ba1) ba2 len2 =
  let
    !len1 = sizeofByteArray# ba1
  in
  isTrue# (len1 <=# len2)
  &&
  isTrue# (0# ==# compareByteArrays# ba1 0# ba2 0# len1)



-- CONSTRAINT PREFIXES


comparable :: S.String; comparable = [S.ascii|comparable|]
appendable :: S.String; appendable = [S.ascii|appendable|]
compappend :: S.String; compappend = [S.ascii|compappend|]
number     :: S.String; number     = [S.ascii|number|]



-- VARIABLES


a :: Var; a = Var [S.ascii|a|] Any
b :: Var; b = Var [S.ascii|b|] Any
c :: Var; c = Var [S.ascii|c|] Any



-- GENERATE VARIABLES


genAny :: Int -> Var
genAny index@(I# i) =
  if index < 26
  then
    runST (ST (\s0 ->
      case newByteArray# 1#                                  s0 of { (# s1, mba #) ->
      case putWord8# mba 0# (plusWord# 0x61## (int2Word# i)) s1 of {    s2         ->
      case unsafeFreezeByteArray# mba                        s2 of { (# s3, ba  #) ->
        (# s3, Var (S.String ba) Any #)
      }}}
    ))
  else
    runST (ST (\s0 ->
      let
        !(extra, I# ltr) = quotRem index 26
        !(I# len)        = 1 + getIndexSize extra
      in
      case newByteArray# len                                   s0 of { (# s1, mba #) ->
      case putWord8# mba 0# (plusWord# 0x61## (int2Word# ltr)) s1 of {    s2         ->
      case writeDigitsAtEnd mba len extra                      s2 of {    s3         ->
      case unsafeFreezeByteArray# mba                          s3 of { (# s4, ba  #) ->
        (# s4, Var (S.String ba) Any #)
      }}}}
    ))



-- GENERATE CONSTRAINTS


genComparable :: Int -> Var; genComparable i = Var (genIndexed comparable i) Comparable
genAppendable :: Int -> Var; genAppendable i = Var (genIndexed appendable i) Appendable
genCompAppend :: Int -> Var; genCompAppend i = Var (genIndexed compappend i) CompAppend
genNumber     :: Int -> Var; genNumber     i = Var (genIndexed number     i) Number



-- GEN INDEXED NAME


genIndexed :: S.String -> Int -> S.String
genIndexed string@(S.String name) index =
  if index <= 0
  then string
  else
    let
      len = sizeofByteArray# name
      end = indexWord8Array# name (len -# 1#)
    in
    if isTrue# (leWord8# 0x30#Word8 end) && isTrue# (leWord8# end 0x39#Word8)
    then
      runST $ ST $ \s0 ->
        let
          !(I# size) = I# len + 1 + getIndexSize index
        in
        case newByteArray# size                s0 of { (# s1, mba #) ->
        case copyByteArray# name 0# mba 0# len s1 of {    s2         ->
        case putWord8# mba len 0x5F## {-_-}    s2 of {    s3         ->
        case writeDigitsAtEnd mba size index   s3 of {    s4         ->
        case unsafeFreezeByteArray# mba        s4 of { (# s5, ba  #) ->
          (# s5, S.String ba #)
        }}}}}
    else
      runST $ ST $ \s0 ->
        let
          !(I# size) = I# len + getIndexSize index
        in
        case newByteArray# size                s0 of { (# s1, mba #) ->
        case copyByteArray# name 0# mba 0# len s1 of {    s2         ->
        case writeDigitsAtEnd mba size index   s2 of {    s3         ->
        case unsafeFreezeByteArray# mba        s3 of { (# s4, ba  #) ->
          (# s4, S.String ba #)
        }}}}


{-# INLINE putWord8# #-}
putWord8# :: MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
putWord8# mba off w s =
  writeWord8Array# mba off (wordToWord8# w) s



-- HELPERS


getIndexSize :: Int -> Int
getIndexSize n
  | n < 10    = 1
  | n < 100   = 2
  | n < 1000  = 3
  | n < 10000 = 4
  | otherwise = loop (div n 10000) 4
  where
    loop x size
      | x < 10    = size + 1
      | x < 100   = size + 2
      | x < 1000  = size + 3
      | x < 10000 = size + 4
      | otherwise = loop (div x 10000) (size + 4)


writeDigitsAtEnd :: MutableByteArray# s -> Int# -> Int -> State# s -> State# s
writeDigitsAtEnd mba oldOffset n s0 =
  let
    (q,r)      = quotRem n 10
    !newOffset = oldOffset -# 1#
    !(I# i)    = 0x30 + r
  in
  case putWord8# mba newOffset (int2Word# i) s0 of
    s1 ->
      if q <= 0
      then s1
      else writeDigitsAtEnd mba newOffset q s1



-- BINARY FORMAT


eVar :: Var -> E.Builder
eVar (Var name (Constraint con)) =
  E.string8 name <> E.u8# con


dVar :: D.Decoder Var
dVar =
  do  name <- D.string8
      D.u8# (\n -> Var name (Constraint n))
