{-# LANGUAGE DeriveDataTypeable #-}
module SourceSyntax.Declaration where

import Data.Data
import qualified SourceSyntax.Expression as Expr
import Types.Types

data Declaration tipe var
    = Definition (Expr.Def tipe var)
    | Datatype String [X] [(String,[Type])]
    | TypeAlias String [X] Type
    | ImportEvent String (Expr.LExpr tipe var) String Type
    | ExportEvent String String Type
    | Fixity Assoc Int String
      deriving (Eq, Show, Data, Typeable, Show)

data Assoc = L | N | R
             deriving (Eq, Show, Data, Typeable, Show)
