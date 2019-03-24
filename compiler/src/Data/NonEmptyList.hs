module Data.NonEmptyList
  ( List(..)
  , toList
  )
  where



-- LIST


data List a =
  List a [a]


toList :: List a -> [a]
toList (List x xs) =
  x:xs



-- INSTANCES


instance Functor List where
  fmap func (List x xs) = List (func x) (map func xs)


instance Traversable List where
  traverse func (List x xs) = List <$> func x <*> traverse func xs


instance Foldable List where
  foldr step state (List x xs) = step x (foldr step state xs)
  foldl step state (List x xs) = foldl step (step state x) xs
  foldl1 step      (List x xs) = foldl step x xs
