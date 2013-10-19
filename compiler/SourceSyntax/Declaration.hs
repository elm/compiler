{-# LANGUAGE DeriveDataTypeable #-}
module SourceSyntax.Declaration where

import Data.Data
import qualified SourceSyntax.Expression as Expr
import SourceSyntax.Type
import SourceSyntax.PrettyPrint
import Text.PrettyPrint as P

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

instance Pretty (Declaration t v) where
  pretty decl =
    case decl of
      Definition def -> pretty def

      Datatype tipe tvars ctors ->
          P.hang (P.text "data" <+> P.text tipe <+> P.hsep (map P.text tvars)) 4
               (P.sep $ zipWith join ("=" : repeat "|") ctors)
          where
            join c ctor = P.text c <+> prettyCtor ctor
            prettyCtor (name, tipes) =
                P.hang (P.text name) 2 (P.sep (map prettyParens tipes))

      TypeAlias name tvars tipe ->
          let alias = P.text name <+> P.hsep (map P.text tvars) in
          P.hang (P.text "type" <+> alias <+> P.equals) 4 (pretty tipe)

      -- TODO: actually write out the other cases. They are currently unused, but
      -- this is probably going to be a bug someday.
      _ -> P.text (show decl)