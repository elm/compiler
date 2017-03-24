{-# OPTIONS_GHC -Wall #-}
module Reporting.Annotation
  ( Annotated(..)
  , Located
  , Commented
  , at, merge, sameAs
  , map, drop
  , listToDict
  )
  where


import Prelude hiding (drop, map)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Reporting.Region as R



-- ANNOTATION


data Annotated annotation a =
  A annotation a


type Located a =
  Annotated R.Region a


type Commented a =
  Annotated (R.Region, Maybe Text) a



-- CREATE


at :: R.Position -> R.Position -> a -> Located a
at start end value =
  A (R.Region start end) value


merge :: Located a -> Located b -> value -> Located value
merge (A region1 _) (A region2 _) value =
  A (R.merge region1 region2) value


sameAs :: Annotated info a -> b -> Annotated info b
sameAs (A annotation _) value =
  A annotation value



-- MANIPULATE


map :: (a -> b) -> Annotated info a -> Annotated info b
map f (A info value) =
  A info (f value)


drop :: Annotated info a -> a
drop (A _ value) =
  value



-- ANALYZE


listToDict :: (Ord key) => (a -> key) -> [Annotated i a] -> Map.Map key (NonEmpty i)
listToDict toKey list =
  let
    add dict (A info value) =
      Map.insertWith (<>) (toKey value) (info :| []) dict
  in
    List.foldl' add Map.empty list
