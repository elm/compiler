module ThreadSafe.Fork
  ( SafeMVar
  , await
  --
  , fork_
  , fork
  , forkWithKey
  --
  , Context(..)
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception (Exception(..), SomeException, throwIO)
import qualified Data.Map as Map

import qualified Crash



-- SAFE MVAR


newtype SafeMVar a =
  SafeMVar (MVar (Either SomeException a))


await :: SafeMVar a -> IO a
await (SafeMVar mvar) =
  do  result <- readMVar mvar
      case result of
        Right a -> return a
        Left  x -> throwIO x



-- FORK


fork_ :: IO a -> IO (SafeMVar a)
fork_ work =
  do  mvar <- newEmptyMVar
      _ <- forkIO $ putMVar mvar =<< (fmap Right work `Crash.recover` \x -> return (Left x))
      return (SafeMVar mvar)


fork :: (Context k) => k -> IO a -> IO (SafeMVar a)
fork key work =
  do  mvar <- newEmptyMVar
      _ <- forkIO $ putMVar mvar =<< (fmap Right work `Crash.recover` recrash)
      return (SafeMVar mvar)
  where
    recrash x =
      case fromException x of
        Just (ContextException _ _) -> return $ Left x
        Nothing                     -> return $ Left $ toException $ ContextException x (toContextChars key)


{-# INLINE forkWithKey #-}
forkWithKey :: (Context k) => (k -> a -> IO b) -> Map.Map k a -> IO (Map.Map k (SafeMVar b))
forkWithKey func dict =
  Map.traverseWithKey (\k v -> fork k (func k v)) dict



-- THREADED EXCEPTION


class Context k where
  toContextChars :: k -> [Char]


data ContextException =
  ContextException SomeException String


instance Show ContextException where
  show (ContextException err ctx) =
    show err ++ " (" ++ ctx ++ ")"


instance Exception ContextException


