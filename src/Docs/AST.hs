{-# OPTIONS_GHC -Wall #-}
module Docs.AST
  ( Centralized(..), Checked(..)
  , Value(..), RawValue, GoodValue
  , Alias(..), RawAlias, GoodAlias
  , Union(..), RawUnion, GoodUnion
  )
  where


import qualified Data.Map as Map
import Data.Text (Text)

import qualified Elm.Compiler.Type as Type
import qualified Reporting.Annotation as A



-- DOCS


data Centralized =
  Centralized
    { comment :: Text
    , values :: Map.Map Text (A.Located RawValue)
    , aliases :: Map.Map Text (A.Located RawAlias)
    , unions :: Map.Map Text (A.Located RawUnion)
    }


data Checked =
  Checked
    { _comment :: Text
    , _values :: Map.Map Text GoodValue
    , _aliases :: Map.Map Text GoodAlias
    , _unions :: Map.Map Text GoodUnion
    }



-- VALUES


data Value comment tipe =
  Value
    { valueComment :: comment
    , valueType :: tipe
    , valueAssocPrec :: Maybe (Text,Int)
    }


type RawValue = Value (Maybe Text) (Maybe Type.Type)


type GoodValue = Value Text Type.Type



-- ALIAS


data Alias comment =
  Alias
    { aliasComment :: comment
    , aliasArgs :: [Text]
    , aliasType :: Type.Type
    }


type RawAlias = Alias (Maybe Text)


type GoodAlias = Alias Text



-- UNION


data Union comment =
  Union
    { unionComment :: comment
    , unionArgs :: [Text]
    , unionCases :: [(Text, [Type.Type])]
    }


type RawUnion = Union (Maybe Text)


type GoodUnion = Union Text
