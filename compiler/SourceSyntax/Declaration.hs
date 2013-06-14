{-# LANGUAGE DeriveDataTypeable #-}
module SourceSyntax.Declaration where

import Data.Data
import qualified SourceSyntax.Expression as Expr
import Types.Types

data Declaration
    = Definition Expr.Def
    | Datatype String [X] [(String,[Type])]
    | TypeAlias String [X] Type
    | ImportEvent String Expr.LExpr String Type
    | ExportEvent String String Type
    | Fixity Int String
      deriving (Eq, Show, Data, Typeable)
