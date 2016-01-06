module Reporting.Annotation where

import Prelude hiding (map)
import qualified Reporting.Region as R
import qualified Data.String as String


-- ANNOTATION

data Annotated annotation a
    = A annotation a
    deriving (Eq)


type Located a =
    Annotated R.Region a


type Commented a =
    Annotated (R.Region, Maybe String) a


class AnnotatedShow annotation where
  annotatedShow :: annotation -> String


instance AnnotatedShow R.Region where
    annotatedShow r =
        String.unwords
            [ "at"
            , show (R.line $ R.start r)
            , show (R.column $ R.start r)
            , show (R.line $ R.end r)
            , show (R.column $ R.end r)
            ]


instance (AnnotatedShow ann, Show a) => Show (Annotated ann a) where
    showsPrec p (A ann a) = showParen (p > 10) $
        showString $ String.unwords
            [ annotatedShow ann
            , showsPrec 99 a ""
            ]


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
