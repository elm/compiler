{-# LANGUAGE ExtendedLiterals, MagicHash #-}
module AST.Prim.Operator
  ( Precedence(..)
  , Associativity(..)
  , ePrecedence, dPrecedence
  , eAssociativity, dAssociativity
  )
  where


import Prelude hiding (Either(..))
import Data.Coerce (coerce)

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E



-- BINOP STUFF


newtype Precedence = Precedence Int
  deriving (Eq, Ord)


data Associativity
  = Left
  | Non
  | Right
  deriving (Eq)



-- BINARY


dPrecedence :: D.Decoder Precedence
dPrecedence =
  coerce D.int


ePrecedence :: Precedence -> E.Builder
ePrecedence (Precedence n) =
  E.int n


dAssociativity :: D.Decoder Associativity
dAssociativity =
  do  n <- D.u8
      case n of
        0 -> return Left
        1 -> return Non
        2 -> return Right
        _ -> D.expecting "Associativity"


eAssociativity :: Associativity -> E.Builder
eAssociativity assoc =
  case assoc of
    Left  -> E.u8# 0#Word8
    Non   -> E.u8# 1#Word8
    Right -> E.u8# 2#Word8
