{-# OPTIONS_GHC -Wall #-}
module Elm.Interface
  ( Interfaces
  , Interface(..)
  , Binop(..)
  , fromModule
  )
  where


import Control.Monad (liftM4)
import Data.Binary
import Data.Map ((!))
import qualified Data.Map as Map

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Binop as Binop
import qualified Elm.Name as N



-- INTERFACES


type Interfaces =
  Map.Map ModuleName.Canonical Interface



-- INTERFACE


data Interface =
  Interface
    { _types   :: Map.Map N.Name Can.Annotation
    , _unions  :: Map.Map N.Name Can.Union
    , _aliases :: Map.Map N.Name Can.Alias
    , _binops  :: Map.Map N.Name Binop
    }


data Binop =
  Binop
    { _op_name :: N.Name
    , _op_annotation :: Can.Annotation
    , _op_associativity :: Binop.Associativity
    , _op_precedence :: Binop.Precedence
    }



-- FROM MODULE


fromModule :: Map.Map N.Name Can.Annotation -> Can.Module -> Interface
fromModule types (Can.Module _ _ _ _ unions aliases binops _) =
  Interface
    { _types = types
    , _unions = unions
    , _aliases = aliases
    , _binops = Map.map (toOp types) binops
    }


toOp :: Map.Map N.Name Can.Annotation -> Can.Binop -> Binop
toOp types (Can.Binop_ associativity precedence name) =
  Binop name (types ! name) associativity precedence



-- BINARY


instance Binary Interface where
  get =
    liftM4 Interface get get get get

  put (Interface a b c d) =
    put a >> put b >> put c >> put d


instance Binary Binop where
  get =
    liftM4 Binop get get get get

  put (Binop a b c d) =
    put a >> put b >> put c >> put d

