{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Guid (guid, set, run, runAt, GuidCounter) where

import Control.Monad.State (evalState, State, get, put)

-- Wrapper around State monad.
newtype GuidCounter a = GC { runGC :: State Int a }
    deriving (Monad)

-- Get the next GUID, incrementing the counter.
guid :: GuidCounter Int
guid = GC $ do n <- get
               put (n + 1)
               return n

set n = GC (put n)

run = runAt 0
runAt n x = evalState (runGC x) n