module Docs.AST where

import qualified Data.Map as Map
import Data.Text (Text)

import qualified Elm.Compiler.Type as Type
import qualified Reporting.Annotation as A


-- FULL DOCUMENTATION

data Docs t =
  Docs
    { comment :: Text
    , aliases :: Map.Map Text (A.Located Alias)
    , types :: Map.Map Text (A.Located Union)
    , values :: Map.Map Text (A.Located (Value t))
    }


type Centralized = Docs (Maybe Type.Type)

type Checked = Docs Type.Type


-- VALUE DOCUMENTATION

data Alias = Alias
    { aliasComment :: Maybe Text
    , aliasArgs :: [Text]
    , aliasType :: Type.Type
    }


data Union = Union
    { unionComment :: Maybe Text
    , unionArgs :: [Text]
    , unionCases :: [(Text, [Type.Type])]
    }


data Value t = Value
    { valueComment :: Maybe Text
    , valueType :: t
    , valueAssocPrec :: Maybe (Text,Int)
    }
