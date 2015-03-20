{-# OPTIONS_GHC -Wall #-}
module AST.Declaration where

import Data.Binary
import qualified AST.Expression.Source as Source
import qualified AST.Expression.Valid as Valid
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Type as T
import qualified AST.Variable as Var
import AST.PrettyPrint
import Text.PrettyPrint as P


-- DECLARATIONS

data Declaration' portType def var
    = Definition def
    | Datatype String [String] [(String, [T.Type var])]
    | TypeAlias String [String] (T.Type var)
    | Port String portType
    | Fixity Assoc Int String


data Assoc = L | N | R
    deriving (Eq)


-- DECLARATION PHASES

type SourceDecl =
  Declaration' T.RawType Source.Def Var.Raw


type ValidDecl =
  Declaration' T.RawType Valid.Def Var.Raw


type CanonicalDecl =
  Declaration' (T.PortType Var.Canonical) Canonical.Def Var.Canonical


-- BINARY CONVERSION

instance Binary Assoc where
    get =
      do  n <- getWord8
          return $ case n of
            0 -> L
            1 -> N
            2 -> R
            _ -> error "Error reading valid associativity from serialized string"

    put assoc =
      putWord8 $
        case assoc of
          L -> 0
          N -> 1
          R -> 2


-- PRETTY STRINGS

assocToString :: Assoc -> String
assocToString assoc =
    case assoc of
      L -> "left"
      N -> "non"
      R -> "right"


instance (Pretty portType, Pretty def, Pretty var, Var.ToString var) =>
    Pretty (Declaration' portType def var) where
  pretty decl =
    case decl of
      Definition def -> pretty def

      Datatype tipe tvars ctors ->
          P.hang
              (P.text "type" <+> P.text tipe <+> P.hsep (map P.text tvars))
              4
              (P.sep $ zipWith (<+>) seperators (map prettyCtor ctors))
        where
          seperators =
              map P.text ("=" : repeat "|")

          prettyCtor (name, tipes) =
              P.hang (P.text name) 2 (P.sep (map T.prettyParens tipes))

      TypeAlias name tvars tipe ->
          P.hang
              (P.text "type" <+> P.text "alias" <+> name' <+> P.equals)
              4
              (pretty tipe)
        where
          name' =
              P.text name <+> P.hsep (map P.text tvars)

      Port name tipe ->
          P.text "port" <+> P.text name <+> P.colon <+> pretty tipe

      Fixity assoc prec op ->
          P.text "infix" <> assoc' <+> P.int prec <+> P.text op
        where
          assoc' =
              case assoc of
                L -> P.text "l"
                N -> P.empty
                R -> P.text "r"
