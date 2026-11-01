{-# LANGUAGE BinaryLiterals, ExtendedLiterals, MagicHash #-}
module AST.Prim.TypeMask
  ( Mask
  --
  , isComparable
  , isNumber
  --
  , int, float, bool, char, string, list, never
  , task, router, cmd, sub
  , vec2, vec3, vec4, mat4, texture, shader
  , unit, pair, triple, pairOf, tripleOf
  , empty, record, recordOf
  --
  , args
  , var
  --
  , lambda
  , cycle
  --
  , variants, enum, vector, box, void
  --
  , eMask
  , dMask
  )
  where


import Prelude hiding (cycle)
import Data.Bits ((.&.))
import qualified Data.List as List
import GHC.Exts (isTrue#)
import GHC.Prim
import GHC.Word (Word8(..))

import qualified Array as A
import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Graph

import qualified AST.Prim.Fields as F
import qualified AST.Prim.Tag as Tag
import qualified AST.Prim.TypeName as T
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg



-- MASK
--
-- Fast way to detect if a type matches various constrained type variables.
-- The meaning of the bits are:
--
--   ┌── comparable
--   │┌─ number
--   00
--
-- This allows us to check type constraints with a small number of bits.


newtype Mask = Mask { _demask :: Word8 }
  deriving (Eq)



-- CHECKS


isComparable :: Mask -> Bool
isNumber     :: Mask -> Bool

isComparable (Mask (W8# w)) = isTrue# (andWord8# w 0b10#Word8 `neWord8#` 0#Word8)
isNumber     (Mask (W8# w)) = isTrue# (andWord8# w 0b01#Word8 `neWord8#` 0#Word8)



-- PRIMITIVES


int    :: Mask; int    = Mask 0b11
float  :: Mask; float  = Mask 0b11
bool   :: Mask; bool   = Mask 0b00
char   :: Mask; char   = Mask 0b10
string :: Mask; string = Mask 0b10
list   :: Mask; list   = Mask 0b10
never  :: Mask; never  = Mask 0b00

task   :: Mask; task   = Mask 0b00
router :: Mask; router = Mask 0b00
cmd    :: Mask; cmd    = Mask 0b00
sub    :: Mask; sub    = Mask 0b00

vec2    :: Mask; vec2    = Mask 0b00
vec3    :: Mask; vec3    = Mask 0b00
vec4    :: Mask; vec4    = Mask 0b00
mat4    :: Mask; mat4    = Mask 0b00
texture :: Mask; texture = Mask 0b00
shader  :: Mask; shader  = Mask 0b00



-- TUPLES


unit   :: Mask
pair   :: Mask
triple :: Mask

unit   = Mask 0b00
pair   = Mask 0b10
triple = Mask 0b10


pairOf :: (a -> Mask) -> a -> a -> Mask
pairOf func a b =
  Mask $ coerce pair |&| func a |&| func b


tripleOf :: (a -> Mask) -> a -> a -> a -> Mask
tripleOf func a b c =
  Mask $ coerce triple |&| func a |&| func b |&| func c



-- RECORD
--
-- Record are not comparable because the field order in type aliases cannot be
-- used. Field order does not matter to the type system, so when two records
-- with different field orders meet, exploring the fields alphabetically is the
-- only viable option. This would be quite unexpected behavior, likely causeing
-- problems for many people.


empty  :: Mask
record :: Mask

empty  = Mask 0b00
record = Mask 0b00


recordOf :: (a -> Mask) -> F.Fields a -> Mask
recordOf func fields =
  Mask $ F.foldr (\t m -> m |&| func t) (coerce record) fields



-- TYPE ARGS
--
-- Custom types and type aliases produce a mask to summarize their details.
-- This needs to be combined with their type parameters to get the final mask
-- for a given concrete type.
--
-- NOTE: the current approach means that phantom types are considered when
-- determining if a custom type is equatable or hashable. This is a bit more
-- strict than it needs to be, but the tradeoff in implementation performance
-- and complexity suggests that this is an alright balance.


args :: Mask -> (a -> Mask) -> A.Array a -> Mask
args (Mask mask) func xs =
  Mask $ A.foldl (\x m -> m |&| func x) mask xs


{-| A type variable can combine successfully with any other mask. -}
var :: Mask
var =
  Mask 0b11



-- LAMBDA
--
-- Functions do not match any type constraint.


lambda :: Mask
lambda =
  Mask 0b00



-- CYCLE


cycle :: (Graph.Node k v -> Mask) -> Graph.Component k v -> Mask
cycle func component =
  Graph.withComponent component $ \node nodes ->
    Mask $ List.foldl' (\m a -> m |&| func a) (_demask (func node)) nodes



-- UNION TYPES
--
-- Figure out masks for different categories of union types.


variants :: (a -> Mask) -> (v -> [a]) -> Tag.Table v -> Mask
variants func unpack table =
    Mask $ Tag.foldl go 0b00 table
  where
    go v m =
      ands func m (unpack v)


enum :: Mask
enum =
  Mask 0b00


vector :: (a -> Mask) -> a -> a -> [a] -> Mask
vector func a b cs =
  Mask $ ands func 0b00 cs |&| func a |&| func b


box :: (a -> Mask) -> a -> Mask
box func a =
  Mask $ 0b00 |&| func a


void :: ModuleName.Canonical -> T.Name -> Mask
void h n
  | isNotKernel h = defaultVoidMask
  | n == T.bool   = bool
  | n == T.char   = char
  | n == T.string = string
  | n == T.int    = Mask 0b11
  | n == T.float  = Mask 0b11
  | otherwise     = defaultVoidMask
  where
    defaultVoidMask = Mask 0b00


{-# INLINE isNotKernel #-}
isNotKernel :: ModuleName.Canonical -> Bool
isNotKernel (ModuleName.Canonical pkg _) =
  not (Pkg.isKernel pkg)



-- HELPERS


{-# INLINE (|&|) #-}
(|&|) :: Word8 -> Mask -> Word8
(|&|) mask (Mask m) =
  mask .&. m


{-# INLINE ands #-}
ands :: (a -> Mask) -> Word8 -> [a] -> Word8
ands func mask values =
  List.foldl' (\m x -> m |&| func x) mask values



-- ENCODE/DECODE


eMask :: Mask -> E.Builder
eMask (Mask mask) =
  E.u8 mask


dMask :: D.Decoder Mask
dMask =
  Mask <$> D.u8

