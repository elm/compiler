module Data.Map.Utils
  ( fromKeys
  , fromKeysA
  , any
  )
  where


import Prelude hiding (any)
import qualified Data.Map as Map
import Data.Map.Internal (Map(..))



-- FROM KEYS


fromKeys :: (Ord k) => (k -> v) -> [k] -> Map.Map k v
fromKeys toValue keys =
  Map.fromList $ map (\k -> (k, toValue k)) keys


fromKeysA :: (Applicative f, Ord k) => (k -> f v) -> [k] -> f (Map.Map k v)
fromKeysA toValue keys =
  Map.fromList <$> traverse (\k -> (,) k <$> toValue k) keys



-- ANY


{-# INLINE any #-}
any :: (v -> Bool) -> Map.Map k v -> Bool
any isGood = go
  where
    go Tip = False
    go (Bin _ _ v l r) = isGood v || go l || go r
