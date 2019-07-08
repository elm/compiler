{-# LANGUAGE BangPatterns #-}
module BackgroundWriter
  ( Scope
  , withScope
  , writeBinary
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import qualified Data.Binary as Binary
import Data.Foldable (traverse_)

import qualified File



-- BACKGROUND WRITER


newtype Scope =
  Scope (MVar [MVar ()])


withScope :: (Scope -> IO a) -> IO a
withScope callback =
  do  workList <- newMVar []
      result <- callback (Scope workList)
      mvars <- takeMVar workList
      traverse_ takeMVar mvars
      return result


writeBinary :: (Binary.Binary a) => Scope -> FilePath -> a -> IO ()
writeBinary (Scope workList) path value =
  do  mvar <- newEmptyMVar
      _ <- forkIO (File.writeBinary path value >> putMVar mvar ())
      oldWork <- takeMVar workList
      let !newWork = mvar:oldWork
      putMVar workList newWork

