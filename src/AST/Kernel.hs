{-# OPTIONS_GHC -Wall #-}
module AST.Kernel
  ( Info(..)
  , Chunk(..)
  )
  where


import Control.Monad (liftM, liftM2)
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.Text as Text

import qualified AST.Module.Name as ModuleName



-- INFO


data Info =
  Info
    { _imports :: [(ModuleName.Raw, Text.Text)]
    , _chunks :: [Chunk]
    }


data Chunk
  = JS BS.ByteString
  | Var ModuleName.Raw Text.Text
  | Field Text.Text
  | Debug Bool



-- BINARY


instance Binary Info where
  put (Info imports chunks) =
    put imports >> put chunks

  get =
    liftM2 Info get get


instance Binary Chunk where
  put chunk =
    case chunk of
      JS bytes ->
        putWord8 0 >> put bytes

      Var home name ->
        putWord8 1 >> put home >> put name

      Field field ->
        putWord8 2 >> put field

      Debug True ->
        putWord8 3

      Debug False ->
        putWord8 4

  get =
    do  word <- getWord8
        case word of
          0 -> liftM JS get
          1 -> liftM2 Var get get
          2 -> liftM Field get
          3 -> return (Debug True)
          4 -> return (Debug False)
          _ -> error "problem deserializing Parse.Kernel.Chunk"
