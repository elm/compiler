{-# LANGUAGE EmptyDataDecls, FlexibleInstances, MagicHash #-}
module Elm.Float
  ( Float
  , fromAddr
  , toBuilder
  --
  , encode
  , decode
  )
  where


import Prelude hiding (Float)
import qualified Data.ByteString.Builder as B
import qualified Data.Utf8 as Utf8
import GHC.Prim

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E



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


encode :: Float -> E.Builder
encode =
  Utf8.encode8


decode :: D.Decoder Float
decode =
  Utf8.decode8


