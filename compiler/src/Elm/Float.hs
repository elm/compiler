{-# LANGUAGE EmptyDataDecls, FlexibleInstances, MagicHash #-}
module Elm.Float
  ( Float
  , fromAddr
  , toBuilder
  )
  where


import Prelude hiding (Float)
import Data.Binary (Binary, get, put)
import qualified Data.ByteString.Builder as B
import qualified Data.Utf8 as Utf8
import GHC.Prim



-- FLOATS


type Float =
  Utf8.Utf8 ELM_FLOAT


data ELM_FLOAT



-- HELPERS


fromAddr :: Addr# -> Addr# -> IO Float
fromAddr =
  Utf8.fromAddr


{-# INLINE toBuilder #-}
toBuilder :: Float -> B.Builder
toBuilder =
  Utf8.toBuilder



-- BINARY


instance Binary (Utf8.Utf8 ELM_FLOAT) where
  get = Utf8.getUnder256
  put = Utf8.putUnder256
