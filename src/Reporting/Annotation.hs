{-# OPTIONS_GHC -Wall #-}
module Reporting.Annotation
  ( Located(..)
  , at, merge
  , map
  , toValue
  , toRegion
  , traverse
  )
  where


import Prelude hiding (map, traverse)
import qualified Reporting.Region as R



-- ANNOTATION


data Located a =
  At R.Region a



-- CREATE


at :: R.Position -> R.Position -> a -> Located a
at start end value =
  At (R.Region start end) value


merge :: Located a -> Located b -> value -> Located value
merge (At region1 _) (At region2 _) value =
  At (R.merge region1 region2) value



-- MANIPULATE


map :: (a -> b) -> Located a -> Located b
map f (At info value) =
  At info (f value)


toValue :: Located a -> a
toValue (At _ value) =
  value


toRegion :: Located a -> R.Region
toRegion (At region _) =
  region


traverse :: (Functor f) => (a -> f b) -> Located a -> f (Located b)
traverse func (At region value) =
  At region <$> func value
