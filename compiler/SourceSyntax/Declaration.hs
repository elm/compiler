{-# LANGUAGE DeriveDataTypeable #-}
module SourceSyntax.Declaration where

import Data.Data
import qualified SourceSyntax.Expression as Expr
import SourceSyntax.Type

data Declaration tipe var
    = Definition (Expr.Def tipe var)
    | Datatype String [String] [(String,[Type])]
    | TypeAlias String [String] Type
    | ImportEvent String (Expr.LExpr tipe var) String Type
    | ExportEvent String String Type
    | Fixity Assoc Int String
      deriving (Eq, Show)

data Assoc = L | N | R
             deriving (Eq)

instance Show Assoc where
    show assoc =
        case assoc of
          L -> "left"
          N -> "non"
          R -> "right"