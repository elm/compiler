{-# LANGUAGE Rank2Types #-}
module Reporting.Task
  ( Task
  , run
  , throw
  , mapError
  --
  , io
  , mio
  , eio
  )
  where



-- TASKS


newtype Task x a =
  Task
  (
    forall result. (a -> IO result) -> (x -> IO result) -> IO result
  )


run :: Task x a -> IO (Either x a)
run (Task task) =
  task (return . Right) (return . Left)


throw :: x -> Task x a
throw x =
  Task $ \_ err -> err x


mapError :: (x -> y) -> Task x a -> Task y a
mapError func (Task task) =
  Task $ \ok err ->
    task ok (err . func)



-- IO


{-# INLINE io #-}
io :: IO a -> Task x a
io work =
  Task $ \ok _ -> work >>= ok


mio :: x -> IO (Maybe a) -> Task x a
mio x work =
  Task $ \ok err ->
    do  result <- work
        case result of
          Just a -> ok a
          Nothing -> err x


eio :: (x -> y) -> IO (Either x a) -> Task y a
eio func work =
  Task $ \ok err ->
    do  result <- work
        case result of
          Right a -> ok a
          Left x -> err (func x)



-- INSTANCES


instance Functor (Task x) where
  {-# INLINE fmap #-}
  fmap func (Task taskA) =
    Task $ \ok err ->
      let
        okA arg = ok (func arg)
      in
      taskA okA err


instance Applicative (Task x) where
  {-# INLINE pure #-}
  pure a =
    Task $ \ok _ -> ok a

  {-# INLINE (<*>) #-}
  (<*>) (Task taskFunc) (Task taskArg) =
    Task $ \ok err ->
      let
        okFunc func =
          let
            okArg arg = ok (func arg)
          in
          taskArg okArg err
      in
      taskFunc okFunc err


instance Monad (Task x) where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  (>>=) (Task taskA) callback =
    Task $ \ok err ->
      let
        okA a =
          case callback a of
            Task taskB -> taskB ok err
      in
      taskA okA err
