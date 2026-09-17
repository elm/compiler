{-# LANGUAGE MagicHash, QuasiQuotes #-}
module AST.Prim.Operator
  ( Name
  , toString
  , fromString
  , toChars
  --
  , Precedence(..)
  , Associativity(..)
  --
  , add, sub, mul, div
  , eq, ne, lt, gt, le, ge
  , and, or, app, apL, apR
  , cons
  --
  , encode
  , decode
  )
  where


import Prelude hiding (and, or, div)
import qualified Data.String
import GHC.Word (Word8)

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import String (ascii)
import qualified String as S



-- BINOP STUFF


newtype Name =
  Name S.String


newtype Precedence = Precedence Word8
  deriving (Eq, Ord)


data Associativity
  = Left
  | Non
  | Right
  deriving (Eq)




-- CONVERSIONS


toString :: Name -> S.String
toString (Name s) =
  s


fromString :: S.String -> Name
fromString =
  Name


toChars :: Name -> [Char]
toChars (Name s) =
  S.toChars s



-- OPERATORS


add  :: Name; add  = Name [ascii|+|]
sub  :: Name; sub  = Name [ascii|-|]
mul  :: Name; mul  = Name [ascii|*|]
div  :: Name; div  = Name [ascii|/|]
eq   :: Name; eq   = Name [ascii|==|]
ne   :: Name; ne   = Name [ascii|/=|]
lt   :: Name; lt   = Name [ascii|<|]
gt   :: Name; gt   = Name [ascii|>|]
le   :: Name; le   = Name [ascii|<=|]
ge   :: Name; ge   = Name [ascii|>=|]
and  :: Name; and  = Name [ascii|&&|]
or   :: Name; or   = Name [ascii||||]
app  :: Name; app  = Name [ascii|++|]
apL  :: Name; apL  = Name [ascii|<||]
apR  :: Name; apR  = Name [ascii||>|]
cons :: Name; cons = Name [ascii|::|]



-- INSTANCES


instance Eq Name where
  (==) (Name ba1) (Name ba2) =
    S.equal ba1 ba2

instance Ord Name where
  compare (Name ba1) (Name ba2) =
    S.compareFast ba1 ba2

instance Data.String.IsString Name where
  fromString = Name . S.fromChars



-- ENCODE/DECODE


encode :: Name -> E.Builder
encode (Name s) =
  E.string8 s


decode :: D.Decoder Name
decode =
  Name <$> D.string8

