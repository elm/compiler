{-# OPTIONS_GHC -Wall #-}
module Elm.Interface
  ( Interfaces
  , Interface(..)
  , fromModule
  , privatize
  )
  where


import Control.Monad (liftM4)
import Data.Binary
import qualified Data.Map as Map

import qualified AST.Expression.Canonical as Canonical
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified Elm.Name as N



-- INTERFACES


type Interfaces =
  Map.Map ModuleName.Canonical Interface



-- INTERFACE


data Interface =
  Interface
    { _types   :: Map.Map N.Name Type.Canonical
    , _unions  :: Map.Map N.Name Canonical.Union
    , _aliases :: Map.Map N.Name Canonical.Alias
    , _binops  :: Map.Map N.Name Canonical.Binop
    }



-- FROM MODULE


fromModule :: Canonical.Module -> Interface
fromModule (Canonical.Module _ _ _ _ unions aliases binops _) =
  Interface
    { _types   = error "TODO types"
    , _unions  = unions
    , _aliases = aliases
    , _binops  = binops
    }



-- PRIVATIZE


privatize :: Interface -> Maybe Interface
privatize (Interface _ unions aliases _) =
  if Map.null unions && Map.null aliases then
    Nothing

  else
    Just $ Interface
      { _types    = Map.empty
      , _unions   = unions
      , _aliases  = aliases
      , _binops   = Map.empty
      }



-- BINARY


instance Binary Interface where
  get =
    liftM4 Interface get get get get

  put (Interface a b c d) =
    put a >> put b >> put c >> put d
