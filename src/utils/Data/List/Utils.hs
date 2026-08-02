{-# LANGUAGE BangPatterns, MagicHash #-}
module Data.List.Utils
  ( foldl
  , mmap
  , mtraverse
  , mindexes
  , traverse#
  , traverseMaybe
  --
  , indexedMap
  , mindexedMap
  , indexedTraverse
  , indexedTraverse#
  , mindexedTraverse
  --
  , indexedMap1
  , indexedTraverse1
  , traverse1
  , reverse1
  , snoc1
  , init1
  , last1
  , all1
  , sortBy1
  --
  , consMaybe
  --
  , split
  --
  , check
  , check1
  )
  where


import Prelude hiding (foldl)
import qualified Data.List as List
import GHC.Prim



-- FOLDL


{-# INLINE foldl #-}
foldl :: (a -> s -> s) -> s -> [a] -> s
foldl step =
  List.foldl' (\s a -> step a s)


{-# INLINE mmap #-}
mmap :: Monoid m => (a -> m) -> [a] -> m
mmap func list =
  mconcat (List.map func list)


{-# INLINE mtraverse #-}
mtraverse :: (Applicative f, Monoid m) => (a -> f m) -> [a] -> f m
mtraverse func list =
  mconcat <$> traverse func list


mindexes :: Monoid m => (Word -> m) -> [a] -> m
mindexes func =
    loop 0
  where
    loop !i list =
      case list of
        []   -> mempty
        _:xs -> func i <> loop (i + 1) xs


traverse# :: (a -> State# s -> State# s) -> [a] -> State# s -> State# s
traverse# func =
    loop
  where
    loop list s0 =
      case list of
        []   -> s0
        x:xs -> case func x s0 of { s1 -> loop xs s1 }


traverseMaybe :: Applicative f => (a -> f (Maybe b)) -> [a] -> f [b]
traverseMaybe f =
    List.foldr (liftA2 cons . f) (pure [])
  where
    cons mx xs =
      case mx of
        Nothing -> xs
        Just x  -> x:xs



-- INDEXED


indexedMap :: (Num i) => (i -> a -> b) -> [a] -> [b]
indexedMap func xs =
  snd $ List.mapAccumL (\i x -> (i+1, func i x)) 0 xs


mindexedMap :: (Num i, Monoid m) => (i -> a -> m) -> [a] -> m
mindexedMap func =
    go 0
  where
    go i list =
      case list of
        [] -> mempty
        x:xs -> func i x <> go (i + 1) xs


indexedTraverse :: (Applicative f, Num i) => (i -> a -> f b) -> [a] -> f [b]
indexedTraverse func xs =
  sequenceA (indexedMap func xs)


indexedTraverse# :: (Int# -> a -> State# s -> State# s) -> [a] -> State# s -> State# s
indexedTraverse# func =
    loop 0#
  where
    loop i list s0 =
      case list of
        []   -> s0
        x:xs -> case func i x s0 of { s1 -> loop (i +# 1#) xs s1 }


mindexedTraverse :: (Applicative f, Num i, Monoid m) => (i -> a -> f m) -> [a] -> f m
mindexedTraverse func xs =
  mconcat <$> indexedTraverse func xs



-- INDEXED 1


indexedMap1 :: (Num n) => (n -> a -> b) -> a -> [a] -> (b, [b])
indexedMap1 func x xs =
  ( func 0 x
  , snd $ List.mapAccumL (\i y -> (i+1, func i y)) 1 xs
  )


indexedTraverse1 :: (Applicative f, Num n) => (n -> a -> f b) -> a -> [a] -> f (b, [b])
indexedTraverse1 func x xs =
  (,)
    <$> func 0 x
    <*> sequenceA (snd (List.mapAccumL (\i y -> (i+1, func i y)) 1 xs))



-- TRAVERSE 1


traverse1 :: (Applicative f) => (a -> f b) -> a -> [a] -> f (b,[b])
traverse1 func x xs =
  (,)
    <$> func x
    <*> traverse func xs



-- REVERSE 1


{-# INLINE reverse1 #-}
reverse1 :: a -> [a] -> (a,[a])
reverse1 x xs =
  List.foldl' (\(y,ys) z -> (z,y:ys)) (x,[]) xs



-- SNOC 1


snoc1 :: (a -> [a] -> b) -> [a] -> a -> b
snoc1 func list end =
  case list of
    []   -> func end []
    x:xs -> func x (xs ++ [end])



-- INIT 1


init1 :: a -> [a] -> [a]
init1 x xs =
  case xs of
    [] -> []
    y:ys -> x : init1 y ys



-- LAST 1


last1 :: a -> [a] -> a
last1 x xs =
  case xs of
    [] -> x
    y:ys -> last1 y ys



-- ALL 1


all1 :: (a -> Bool) -> a -> [a] -> Bool
all1 isGood x xs =
  isGood x && List.all isGood xs



-- SORT BY 1


sortBy1 :: (Ord b) => (a -> b) -> a -> [a] -> (a,[a])
sortBy1 toRank x xs =
  let
    comparison a b =
      compare (toRank a) (toRank b)
  in
  case List.sortBy comparison xs of
    [] ->
      (x,[])

    y:ys ->
      case comparison x y of
        LT -> (x, y:ys)
        EQ -> (x, y:ys)
        GT -> (y, List.insertBy comparison x ys)



-- CONS MAYBE


consMaybe :: Maybe a -> [a] -> [a]
consMaybe m list =
  case m of
    Nothing -> list
    Just v  -> v:list



-- SPLIT


split :: (Eq a) => a -> [a] -> [[a]]
split sep list =
  case list of
    [] -> []
    _:_ ->
      let
        (chunk, chunks) = List.foldr step ([],[]) list
      in
      chunk : chunks
  where
    step value (chunk, chunks) =
      if value == sep
      then ([], chunk:chunks)
      else (value:chunk, chunks)



-- CHECK


check :: [Either x a] -> Either (x,[x]) [a]
check =
    goRight []
  where
    goRight revs results =
      case results of
        []   -> Right (List.reverse revs)
        r:rs ->
          case r of
            Right a -> goRight (a:revs) rs
            Left  x -> goLeft x [] rs

    goLeft x xs results =
      case results of
        []   -> Left (reverse1 x xs)
        r:rs ->
          case r of
            Right _ -> goLeft x xs rs
            Left  y -> goLeft y (x:xs) rs


check1 :: Either x a -> [Either x a] -> Either (x,[x]) (a,[a])
check1 =
    goRight []
  where
    goRight revs result results =
      case result of
        Right a ->
          case results of
            []   -> Right (reverse1 a revs)
            r:rs -> goRight (a:revs) r rs

        Left x ->
          goLeft x [] results

    goLeft x xs results =
      case results of
        []   -> Left (reverse1 x xs)
        r:rs ->
          case r of
            Right  _ -> goLeft x xs rs
            Left y -> goLeft y (x:xs) rs

