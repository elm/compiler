{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, FlexibleInstances #-}
module Elm.Float
  ( Float
  , fromPtr
  , toBuilder
  )
  where


import Prelude hiding (Float)
import Data.Binary (Binary, get, put)
import qualified Data.ByteString.Builder as B
import qualified Data.Utf8 as Utf8
import Data.Word (Word8)
import Foreign.Ptr (Ptr)



-- FLOATS


type Float =
  Utf8.Utf8 ELM_FLOAT


data ELM_FLOAT



-- HELPERS


fromPtr :: Ptr Word8 -> Ptr Word8 -> Float
fromPtr =
  Utf8.fromPtr


{-# INLINE toBuilder #-}
toBuilder :: Float -> B.Builder
toBuilder =
  Utf8.toBuilder



-- BINARY


instance Binary (Utf8.Utf8 ELM_FLOAT) where
  get = Utf8.getUnder256
  put = Utf8.putUnder256
