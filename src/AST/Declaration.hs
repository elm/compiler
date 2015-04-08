{-# OPTIONS_GHC -Wall #-}
module AST.Declaration where

import Data.Binary
import qualified AST.Expression.General as General
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
  Declaration' SourcePort Source.Def Var.Raw Source.Expr


type ValidDecl =
  Declaration' ValidPort Valid.Def Var.Raw Valid.Expr


type CanonicalDecl =
  Declaration' CanonicalPort Canonical.Def Var.Canonical Canonical.Expr


-- PORTS

data SourcePort
    = PortAnnotation String T.RawType
    | PortDefinition String Source.Expr


data ValidPort
    = In String T.RawType
    | Out String Valid.Expr T.RawType


newtype CanonicalPort
    = CanonicalPort (General.PortImpl Canonical.Expr Var.Canonical)


validPortName :: ValidPort -> String
validPortName port =
  case port of
    In name _ -> name
    Out name _ _ -> name


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


instance Pretty SourcePort where
  pretty port =
    case port of
      PortAnnotation name tipe ->
          prettyPortType name tipe

      PortDefinition name expr ->
          prettyPortDef name expr


instance Pretty ValidPort where
  pretty port =
    case port of
      In name tipe ->
          prettyPortType name tipe

      Out name expr tipe ->
          prettyPort name expr tipe


instance Pretty CanonicalPort where
  pretty (CanonicalPort impl) =
    case impl of
      General.In name tipe ->
          prettyPortType name tipe

      General.Out name expr tipe ->
          prettyPort name expr tipe

      General.Task name expr tipe ->
          prettyPort name expr tipe



prettyPort :: (Pretty expr, Pretty tipe) => String -> expr -> tipe -> P.Doc
prettyPort name expr tipe =
  P.vcat
    [ prettyPortType name tipe
    , prettyPortDef name expr
    ]


prettyPortType :: (Pretty tipe) => String -> tipe -> P.Doc
prettyPortType name tipe =
    P.text "port" <+> P.text name <+> P.colon <+> pretty tipe


prettyPortDef :: (Pretty expr) => String -> expr -> P.Doc
prettyPortDef name expr =
    P.hang (P.text "port" <+> P.text name <+> P.equals) 4 (pretty expr)