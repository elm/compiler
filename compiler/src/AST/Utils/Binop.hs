{-# OPTIONS_GHC -Wall #-}
module AST.Utils.Binop
  ( Precedence(..)
  , Associativity(..)
  )
  where


import Prelude hiding (Either(..))
import Control.Monad (liftM)
import Data.Binary



-- BINOP STUFF


newtype Precedence = Precedence Int
  deriving (Eq, Ord)


data Associativity
  = Left
  | Non
  | Right
  deriving (Eq)



-- BINARY


instance Binary Precedence where
  get =
    liftM Precedence get

  put (Precedence n) =
    put n


instance Binary Associativity where
  get =
    do  n <- getWord8
        return $
          case n of
            0 -> Left
            1 -> Non
            2 -> Right
            _ -> error "Error reading valid associativity from serialized string"

  put assoc =
    putWord8 $
      case assoc of
        Left  -> 0
        Non   -> 1
        Right -> 2
