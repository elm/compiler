{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Unique (guid, set, run, runAt, Unique) where

import Control.Monad.State (evalState, State, get, put)

-- Wrapper around State monad.
newtype Unique a = U { runU :: State Int a }
    deriving (Monad)

-- Get the next GUID, incrementing the counter.
guid :: Unique Int
guid = U $ do n <- get
              put (n + 1)
              return n

set n = U (put n)

run = runAt 0
runAt n x = evalState (runU x) n