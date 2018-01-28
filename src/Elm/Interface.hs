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
import qualified Reporting.Annotation as A



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
fromModule types (Can.Module _ _ exports _ unions aliases binops _) =
  Interface
    { _types = privatize exports const types
    , _unions = privatize exports toUnion unions
    , _aliases = privatize exports const aliases
    , _binops = privatize exports const (Map.map (toOp types) binops)
    }


privatize :: Can.Exports -> (a -> A.Located Can.Export -> a) -> Map.Map N.Name a -> Map.Map N.Name a
privatize exports func dict =
  case exports of
    Can.ExportEverything ->
      dict

    Can.Export explicitExports ->
      Map.intersectionWith func dict explicitExports


toUnion :: Can.Union -> A.Located Can.Export -> Can.Union
toUnion union@(Can.Union vars _ _ opts) (A.At _ export) =
  case export of
    Can.ExportUnionOpen ->
      union

    Can.ExportUnionClosed ->
      Can.Union vars [] 0 opts

    _ ->
      error "Compiler bug. Should not run into non-union exports here."


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

