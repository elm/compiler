{-# OPTIONS_GHC -Wall #-}
module Docs.AST
  ( Docs(..), Centralized, Checked
  , Entry(..), Raw, Good
  , Union(..)
  , Alias(..)
  , Value(..), RawValue, GoodValue
  )
  where


import qualified Data.Map as Map
import Data.Text (Text)

import qualified AST.Declaration as Decl
import qualified Elm.Compiler.Type as Type
import qualified Reporting.Annotation as A



-- DOCS


data Docs unions aliases values =
  Docs
    { _overview :: Text
    , _unions :: Map.Map Text unions
    , _aliases :: Map.Map Text aliases
    , _values :: Map.Map Text values
    }


type Centralized =
  Docs (Raw Union) (Raw Alias) (Raw RawValue)


type Checked =
  Docs (Good Union) (Good Alias) (Good GoodValue)



-- ENTRY


data Entry comment details =
  Entry
    { _comment :: !comment
    , _details :: !details
    }


type Raw a =
  A.Located (Entry (Maybe Text) a)


type Good a =
  Entry Text a



-- INFO


data Alias =
  Alias [Text] Type.Type


data Union =
  Union [Text] [(Text, [Type.Type])]


data Value tipe
  = Value tipe
  | Infix tipe Decl.Assoc Int


type RawValue = Value (Maybe Type.Type)

type GoodValue = Value Type.Type
