{-# OPTIONS_GHC -Wall #-}
module AST.Exposing
  ( Canonical(..)
  , nothing
  , Raw
  , Exposing(..)
  , closed
  , Entry(..)
  , getName
  )
  where

import Data.Binary (Binary, get, put)
import Data.Text (Text)

import qualified Reporting.Annotation as A



-- CANONICAL


data Canonical =
  Canonical
    { _values :: ![Text]
    , _aliases :: ![Text]
    , _unions :: ![(Text, [Text])]
    }


nothing :: Canonical
nothing =
  Canonical [] [] []



-- RAW


type Raw = Exposing Entry


data Exposing a
  = Open
  | Explicit ![A.Located a]


closed :: Exposing a
closed =
  Explicit []


data Entry
  = Lower !Text
  | Upper !Text (Maybe (Exposing Text))


getName :: Entry -> Text
getName entry =
  case entry of
    Lower name ->
      name

    Upper name _ ->
      name



-- BINARY


instance Binary Canonical where
  get =
    Canonical <$> get <*> get <*> get

  put (Canonical values aliases unions) =
    do  put values
        put aliases
        put unions

