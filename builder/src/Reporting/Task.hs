{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module Reporting.Task
  ( Task
  , run
  , throw
  -- progress
  , notify
  , ask
  -- io
  , io
  , eio
  , withKey
  )
  where


import qualified System.Exit as Exit

import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Progress as Progress



-- TASKS


newtype Task a =
  Task
  (
    forall result.
      Env
      -> (a -> IO result)
      -> (Exit.Exit -> IO result)
      -> IO result
  )


data Env =
  Env
    { _tell :: Progress.Progress -> IO ()
    , _ask  :: D.Doc -> IO Bool
    }


run :: Progress.Reporter -> Task a -> IO ()
run (Progress.Reporter pTell pAsk pEnd) (Task task) =
  task (Env pTell pAsk)
    (\_ ->
        do  pEnd Nothing
            return ()
    )
    (\exit ->
        do  pEnd (Just exit)
            Exit.exitFailure
    )


throw :: Exit.Exit -> Task a
throw exit =
  Task $ \_ _ err -> err exit



-- PROGRESS


notify :: Progress.Progress -> Task ()
notify progress =
  Task $ \(Env pTell _) ok _ -> pTell progress >>= ok


ask :: D.Doc -> Task Bool
ask question =
  Task $ \(Env _ pAsk) ok _ -> pAsk question >>= ok



-- IO


{-# INLINE io #-}
io :: IO a -> Task a
io work =
  Task $ \_ ok _ -> work >>= ok


eio :: (x -> Exit.Exit) -> IO (Either x a) -> Task a
eio func work =
  Task $ \_ ok err ->
    do  result <- work
        case result of
          Right a -> ok a
          Left e -> err (func e)


withKey :: (Progress.Key -> IO (Either Exit.Exit a)) -> Task a
withKey callback =
  Task $ \(Env pTell _) ok err ->
    do  result <- callback (Progress.Key pTell)
        case result of
          Right a -> ok a
          Left e -> err e



-- INSTANCES


instance Functor Task where
  {-# INLINE fmap #-}
  fmap func (Task taskA) =
    Task $ \env ok err ->
      let
        okA arg = ok (func arg)
      in
      taskA env okA err


instance Applicative Task where
  {-# INLINE pure #-}
  pure a =
    Task $ \_ ok _ -> ok a

  {-# INLINE (<*>) #-}
  (<*>) (Task taskFunc) (Task taskArg) =
    Task $ \env ok err ->
      let
        okFunc func =
          let
            okArg arg = ok (func arg)
          in
          taskArg env okArg err
      in
      taskFunc env okFunc err


instance Monad Task where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  (>>=) (Task taskA) callback =
    Task $ \env ok err ->
      let
        okA a =
          case callback a of
            Task taskB -> taskB env ok err
      in
      taskA env okA err
