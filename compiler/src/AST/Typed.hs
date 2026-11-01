module AST.Typed
  ( Module(..)
  , Exports(..)
  )
  where


import qualified Data.Map as Map

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A



-- MODULES


data Module =
  Module
    { _name    :: ModuleName.Canonical
    , _exports :: Exports
    , _docs    :: Src.Docs
    , _decls   :: Can.Decls
    , _types   :: Map.Map T.Name Can.TypeDecl
    , _binops  :: Map.Map Op.Name Can.Binop
    , _style   :: Can.Style
    }


data Exports
  = ExportEverything A.Region
  | Export
      { _e_types  :: Map.Map  T.Name (A.Region, Src.Privacy)
      , _e_values :: Map.Map  N.Name  A.Region
      , _e_binops :: Map.Map Op.Name  A.Region
      }
