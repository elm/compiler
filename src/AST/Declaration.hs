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

data Declaration' port def var expr
    = Definition def
    | Datatype String [String] [(String, [T.Type var])]
    | TypeAlias String [String] (T.Type var)
    | Port port
    | Fixity Assoc Int String


data Assoc = L | N | R
    deriving (Eq)


-- DECLARATION PHASES

type SourceDecl =
  Declaration' RawPort Source.Def Var.Raw Source.Expr


type ValidDecl =
  Declaration' (Port Valid.Expr T.RawType) Valid.Def Var.Raw Valid.Expr


type CanonicalDecl =
  Declaration' CanonicalPort Canonical.Def Var.Canonical Canonical.Expr


-- PORTS

data RawPort
    = PortAnnotation String T.RawType
    | PortDefinition String Source.Expr


data Port expr tipe
    = Inbound String tipe
    | Outbound String expr tipe


type CanonicalPort =
    Port Canonical.Expr (T.PortType Var.Canonical)


getName :: Port expr tipe -> String
getName port =
  case port of
    Inbound name _ -> name
    Outbound name _ _ -> name


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


instance (Pretty port, Pretty def, Pretty var, Var.ToString var, Pretty expr) =>
    Pretty (Declaration' port def var expr) where
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

      Port port ->
          pretty port

      Fixity assoc prec op ->
          P.text "infix" <> assoc' <+> P.int prec <+> P.text op
        where
          assoc' =
              case assoc of
                L -> P.text "l"
                N -> P.empty
                R -> P.text "r"


instance Pretty RawPort where
  pretty port =
    case port of
      PortAnnotation name tipe ->
          P.text "port" <+> P.text name <+> P.colon <+> pretty tipe

      PortDefinition name expr ->
          P.text "port" <+> P.text name <+> P.equals <+> pretty expr


instance (Pretty expr, Pretty tipe) => Pretty (Port expr tipe) where
  pretty port =
    case port of
      Inbound name tipe ->
          P.text "port" <+> P.text name <+> P.colon <+> pretty tipe

      Outbound name expr tipe ->
          P.vcat
            [ P.text "port" <+> P.text name <+> P.colon <+> pretty tipe
            , P.hang (P.text "port" <+> P.text name <+> P.equals) 4 (pretty expr)
            ]