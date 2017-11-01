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

import qualified Elm.Name as N



-- DATA


data Data =
  Data
    { _client :: Content
    , _server :: Maybe Content
    }



-- CONTENT


data Content =
  Content
    { _imports :: [(N.Name, N.Name)]
    , _chunks :: [Chunk]
    }



-- CHUNKS


data Chunk
  = JS BS.ByteString
  | Var N.Name N.Name
  | Field N.Name
  | Debug
  | Prod



-- BINARY


instance Binary Data where
  put (Data a b) =
    put a >> put b

  get =
    liftM2 Data get get


instance Binary Content where
  put (Content a b) =
    put a >> put b

  get =
    liftM2 Content get get


instance Binary Chunk where
  put chunk =
    case chunk of
      JS a    -> putWord8 0 >> put a
      Var a b -> putWord8 1 >> put a >> put b
      Field a -> putWord8 2 >> put a
      Debug   -> putWord8 3
      Prod    -> putWord8 4

  get =
    do  word <- getWord8
        case word of
          0 -> liftM JS get
          1 -> liftM2 Var get get
          2 -> liftM Field get
          3 -> return Debug
          4 -> return Prod
          _ -> error "problem deserializing Parse.Kernel.Chunk"
