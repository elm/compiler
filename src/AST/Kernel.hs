{-# OPTIONS_GHC -Wall #-}
module AST.Kernel
  ( Data(..)
  , Content(..)
  , Chunk(..)
  )
  where


import Control.Monad (liftM, liftM2)
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.Text as Text

import qualified AST.Module.Name as ModuleName



-- DATA


data Data =
  Data
    { _client :: Content
    , _server :: Maybe Content
    }



-- CONTENT


data Content =
  Content
    { _imports :: [(ModuleName.Raw, Text.Text)]
    , _chunks :: [Chunk]
    }



-- CHUNKS


data Chunk
  = JS BS.ByteString
  | Var ModuleName.Raw Text.Text
  | Field Text.Text
  | Debug
  | Prod



-- BINARY


instance Binary Data where
  put (Data client server) =
    put client >> put server

  get =
    liftM2 Data get get


instance Binary Content where
  put (Content imports chunks) =
    put imports >> put chunks

  get =
    liftM2 Content get get


instance Binary Chunk where
  put chunk =
    case chunk of
      JS bytes ->
        putWord8 0 >> put bytes

      Var home name ->
        putWord8 1 >> put home >> put name

      Field field ->
        putWord8 2 >> put field

      Debug ->
        putWord8 3

      Prod ->
        putWord8 4

  get =
    do  word <- getWord8
        case word of
          0 -> liftM JS get
          1 -> liftM2 Var get get
          2 -> liftM Field get
          3 -> return Debug
          4 -> return Prod
          _ -> error "problem deserializing Parse.Kernel.Chunk"
