{-# LANGUAGE MagicHash, QuasiQuotes, TemplateHaskellQuotes #-}
module AST.Prim.Name
  ( Name
  , size
  , toString
  , toBuilder
  , toChars
  --
  , fromString
  , fromAddr
  --
  , ascii
  --
  , true, false, a, b
  , first, second, unit, pair, triple
  , update
  , cons, nil, fromArray
  , identity, negate, todo
  , main
  , replValueToPrint
  --
  , encode
  , decode
  )
  where


import Prelude hiding (negate)
import qualified Data.ByteString.Builder as B
import GHC.Prim
import Language.Haskell.TH (Exp(AppE, ConE))
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified String as S



-- NAME
--
-- The unicode representation of a name, following the normal rules for names.
--


newtype Name =
  Name S.String


size :: Name -> Int
size (Name s) =
  S.size s


{-# INLINE toString #-}
toString :: Name -> S.String
toString (Name s) =
  s


{-# INLINE toBuilder #-}
toBuilder :: Name -> B.Builder
toBuilder (Name s) =
  S.toBuilder s


toChars :: Name -> [Char]
toChars (Name s) =
  S.toChars s



-- FROM STRING


{-# INLINE fromString #-}
fromString :: S.String -> Name
fromString =
  coerce



-- FROM PTR


{-# INLINE fromAddr #-}
fromAddr :: Addr# -> Addr# -> IO Name
fromAddr pos end =
  Name <$> S.fromAddr pos end



-- ASCII LITERALS


ascii :: QuasiQuoter
ascii =
  QuasiQuoter
    { quoteExp  = \s -> AppE (ConE 'Name) <$> S.asciiExp s
    , quotePat  = \_ -> fail "cannot use [ascii| ... |] as a pattern"
    , quoteType = \_ -> fail "cannot use [ascii| ... |] as a type"
    , quoteDec  = \_ -> fail "cannot use [ascii| ... |] as a declaration"
    }



-- NAMES


true      :: Name; true      = Name [S.ascii|True|]
false     :: Name; false     = Name [S.ascii|False|]
a         :: Name; a         = Name [S.ascii|a|]
b         :: Name; b         = Name [S.ascii|b|]
first     :: Name; first     = Name [S.ascii|first|]
second    :: Name; second    = Name [S.ascii|second|]
unit      :: Name; unit      = Name [S.ascii|#0|]
pair      :: Name; pair      = Name [S.ascii|#2|]
triple    :: Name; triple    = Name [S.ascii|#3|]
update    :: Name; update    = Name [S.ascii|update|]
cons      :: Name; cons      = Name [S.ascii|::|]
nil       :: Name; nil       = Name [S.ascii|[]|]
fromArray :: Name; fromArray = Name [S.ascii|fromArray|]
identity  :: Name; identity  = Name [S.ascii|identity|]
negate    :: Name; negate    = Name [S.ascii|negate|]
todo      :: Name; todo      = Name [S.ascii|todo|]
main      :: Name; main      = Name [S.ascii|main|]


replValueToPrint :: Name
replValueToPrint = Name [S.ascii|repl_input_value_|]



-- INSTANCES


instance Eq Name where
  (==) (Name ba1) (Name ba2) =
    S.equal ba1 ba2

instance Ord Name where
  compare (Name ba1) (Name ba2) =
    S.compareFast ba1 ba2



-- ENCODE/DECODE


encode :: Name -> B.Builder
encode (Name s) =
  E.string8 s


decode :: D.Decoder Name
decode =
  Name <$> D.string8

