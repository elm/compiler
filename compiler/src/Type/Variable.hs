{-# LANGUAGE MagicHash #-}
module Type.Variable
  ( Variable
  , Descriptor(..)
  , Content(..)
  , FlatType(..)
  --
  , Rank(..)
  , noRank
  , outermostRank
  , incrRank
  , Mark
  , noMark
  , incrMark
  , occursMark
  , getVarNamesMark
  --
  , mkFlexAny
  , mkFlexNumber
  --
  , flexAny
  , flexComparable
  , flexCompAppend
  --
  , varToFlex
  , varToRigid
  )
  where


import qualified Data.NonEmptyMap as NEM
import GHC.Word (Word32)

import qualified Array as A
import qualified String as S

import qualified AST.Prim.Fields as F
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeMask as M
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Type.UnionFind as UF



-- TYPE VARIABLES
--
-- NOTE: Unnamed flex variables are represented by a T.Var with an empty name,
-- so some special care is needed when assigning names after type inference.


type Variable =
  UF.Point Descriptor


data Descriptor =
  Descriptor
    { _content :: Content
    , _rank :: Rank
    , _mark :: Mark
    , _copy :: Maybe Variable
    }


data Content
  = Flex T.Var
  | Rigid T.Var
  | Structure FlatType
  | Alias ModuleName.Canonical T.Name M.Mask (A.Array Variable) I.Aliased Variable
  | Error


data FlatType
  = App1 ModuleName.Canonical T.Name M.Mask (A.Array Variable)
  | Fun1 Variable Variable
  | Unit1
  | Pair1 Variable Variable
  | Triple1 Variable Variable Variable
  | Empty1
  | Record1 F.Order# (NEM.Map N.Name Variable)
  | ExtRec1 F.Order# (NEM.Map N.Name Variable) Variable



-- RANKS


newtype Rank = Rank Int
  deriving (Eq, Ord)


noRank :: Rank
noRank =
  Rank 0


outermostRank :: Rank
outermostRank =
  Rank 1


{-# INLINE incrRank #-}
incrRank :: Rank -> Rank
incrRank (Rank r) =
  Rank (r + 1)



-- MARKS


newtype Mark = Mark Word32
  deriving (Eq, Ord)


noMark :: Mark
noMark =
  Mark 2


occursMark :: Mark
occursMark =
  Mark 1


getVarNamesMark :: Mark
getVarNamesMark =
  Mark 0


{-# INLINE incrMark #-}
incrMark :: Mark -> Mark
incrMark (Mark mark) =
  Mark (mark + 1)



-- MAKE FLEX VARIABLES


mkFlexAny :: IO Variable
mkFlexAny =
  UF.fresh flexAnyDescriptor


{-# NOINLINE flexAnyDescriptor #-}
flexAnyDescriptor :: Descriptor
flexAnyDescriptor =
  makeDescriptor flexAny


makeDescriptor :: Content -> Descriptor
makeDescriptor content =
  Descriptor content noRank noMark Nothing



-- MAKE FLEX NUMBERS


mkFlexNumber :: IO Variable; mkFlexNumber = UF.fresh flexNumberDescriptor

{-# NOINLINE flexNumberDescriptor  #-}

flexNumberDescriptor :: Descriptor; flexNumberDescriptor = makeDescriptor (Flex (T.Var S.empty T.Number))


{-# NOINLINE flexAny        #-}; flexAny        :: Content; flexAny        = Flex (T.Var S.empty T.Any)
{-# NOINLINE flexComparable #-}; flexComparable :: Content; flexComparable = Flex (T.Var S.empty T.Comparable)
{-# NOINLINE flexCompAppend #-}; flexCompAppend :: Content; flexCompAppend = Flex (T.Var S.empty T.CompAppend)



-- MAKE NAMED VARIABLES


varToFlex :: T.Var -> IO Variable
varToFlex var =
  UF.fresh $ makeDescriptor $ Flex var


varToRigid :: T.Var -> IO Variable
varToRigid var =
  UF.fresh $ makeDescriptor $ Rigid var


