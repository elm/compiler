module Reporting.Annotation where

import Prelude hiding (map)
import qualified Reporting.Region as R



-- ANNOTATION


data Annotated annotation a
    = A annotation a


type Located a =
    Annotated R.Region a


type Commented a =
    Annotated (R.Region, Maybe String) a



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
map f (A annotation value) =
    A annotation (f value)


drop :: Annotated info a -> a
drop (A _ value) =
    value

