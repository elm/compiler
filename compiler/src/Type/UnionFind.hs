{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
module Type.UnionFind
  ( Point
  , fresh
  , union
  , equivalent
  , redundant
  , get
  , set
  , modify
  )
  where


{- This is based on the following implementations:

  - https://hackage.haskell.org/package/union-find-0.2/docs/src/Data-UnionFind-IO.html
  - http://yann.regis-gianas.org/public/mini/code_UnionFind.html

It seems like the OCaml one came first, but I am not sure.

Compared to the Haskell implementation, the major changes here include:

  1. No more reallocating PointInfo when changing the weight
  2. Using the strict modifyIORef

-}


import Control.Monad ( when )
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Word (Word32)



-- POINT


newtype Point a =
  Pt (IORef (PointInfo a))
  deriving Eq


data PointInfo a
  = Info {-# UNPACK #-} !(IORef Word32) {-# UNPACK #-} !(IORef a)
  | Link {-# UNPACK #-} !(Point a)



-- HELPERS


fresh :: a -> IO (Point a)
fresh value =
  do  weight <- newIORef 1
      desc <- newIORef value
      link <- newIORef (Info weight desc)
      return (Pt link)


repr :: Point a -> IO (Point a)
repr point@(Pt ref) =
  do  pInfo <- readIORef ref
      case pInfo of
        Info _ _ ->
          return point

        Link point1@(Pt ref1) ->
          do  point2 <- repr point1
              when (point2 /= point1) $
                do  pInfo1 <- readIORef ref1
                    writeIORef ref pInfo1
              return point2


get :: Point a -> IO a
get point@(Pt ref) =
  do  pInfo <- readIORef ref
      case pInfo of
        Info _ descRef ->
          readIORef descRef

        Link (Pt ref1) ->
          do  link' <- readIORef ref1
              case link' of
                Info _ descRef ->
                  readIORef descRef

                Link _ ->
                  get =<< repr point


set :: Point a -> a -> IO ()
set point@(Pt ref) newDesc =
  do  pInfo <- readIORef ref
      case pInfo of
        Info _ descRef ->
          writeIORef descRef newDesc

        Link (Pt ref1) ->
          do  link' <- readIORef ref1
              case link' of
                Info _ descRef ->
                  writeIORef descRef newDesc

                Link _ ->
                  do  newPoint <- repr point
                      set newPoint newDesc


modify :: Point a -> (a -> a) -> IO ()
modify point@(Pt ref) func =
  do  pInfo <- readIORef ref
      case pInfo of
        Info _ descRef ->
          modifyIORef' descRef func

        Link (Pt ref1) ->
          do  link' <- readIORef ref1
              case link' of
                Info _ descRef ->
                  modifyIORef' descRef func

                Link _ ->
                  do  newPoint <- repr point
                      modify newPoint func


union :: Point a -> Point a -> a -> IO ()
union p1 p2 newDesc =
  do  point1@(Pt ref1) <- repr p1
      point2@(Pt ref2) <- repr p2

      Info w1 d1 <- readIORef ref1
      Info w2 d2 <- readIORef ref2

      if point1 == point2
        then writeIORef d1 newDesc
        else do
          weight1 <- readIORef w1
          weight2 <- readIORef w2

          let !newWeight = weight1 + weight2

          if weight1 >= weight2
            then
              do  writeIORef ref2 (Link point1)
                  writeIORef w1 newWeight
                  writeIORef d1 newDesc
            else
              do  writeIORef ref1 (Link point2)
                  writeIORef w2 newWeight
                  writeIORef d2 newDesc


equivalent :: Point a -> Point a -> IO Bool
equivalent p1 p2 =
  do  v1 <- repr p1
      v2 <- repr p2
      return (v1 == v2)


redundant :: Point a -> IO Bool
redundant (Pt ref) =
  do  pInfo <- readIORef ref
      case pInfo of
        Info _ _ ->
          return False

        Link _ ->
          return True
