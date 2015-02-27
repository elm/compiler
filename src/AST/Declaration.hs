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


data Declaration' wire def var
    = Definition def
    | Datatype String [String] [(String, [T.Type var])]
    | TypeAlias String [String] (T.Type var)
    | Wire wire
    | Fixity Assoc Int String


data Assoc = L | N | R
    deriving (Eq)


data RawWire
    = InputAnnotation String T.RawType
    | OutputAnnotation String T.RawType
    | OutputDefinition String Source.Expr
    | LoopbackAnnotation String T.RawType
    | LoopbackDefinition String Source.Expr


data Wire expr var
    = Output String expr (T.Type var)
    | Input String (T.Type var)
    | Loopback String (Maybe expr) (T.Type var)


type SourceDecl =
  Declaration' RawWire Source.Def Var.Raw


type ValidDecl =
  Declaration' (Wire Valid.Expr Var.Raw) Valid.Def Var.Raw


type CanonicalDecl =
  Declaration' (Wire Canonical.Expr Var.Canonical) Canonical.Def Var.Canonical


wireName :: Wire expr var -> String
wireName wire =
    case wire of
      Input name _ -> name
      Output name _ _ -> name
      Loopback name _ _ -> name


assocToString :: Assoc -> String
assocToString assoc =
    case assoc of
      L -> "left"
      N -> "non"
      R -> "right"


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

instance (Pretty wire, Pretty def, Pretty var, Var.ToString var) =>
    Pretty (Declaration' wire def var) where
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

      Wire wire -> pretty wire

      Fixity assoc prec op ->
          P.text "infix" <> assoc' <+> P.int prec <+> P.text op
        where
          assoc' =
              case assoc of
                L -> P.text "l"
                N -> P.empty
                R -> P.text "r"


instance Pretty RawWire where
  pretty wire =
    case wire of
      InputAnnotation name tipe ->
          prettyWire "input" name ":" tipe

      OutputAnnotation name tipe ->
          prettyWire "output" name ":" tipe

      OutputDefinition name expr ->
          prettyWire "output" name "=" expr

      LoopbackAnnotation name tipe ->
          prettyWire "loopback" name ":" tipe

      LoopbackDefinition name expr ->
          prettyWire "loopback" name "=" expr


instance (Pretty expr, Pretty var, Var.ToString var) => Pretty (Wire expr var) where
  pretty wire =
    case wire of
      Input name tipe ->
          prettyWire "input" name ":" tipe

      Output name expr tipe ->
          P.vcat
            [ prettyWire "output" name ":" tipe
            , prettyWire "output" name "=" expr
            ]

      Loopback name maybeExpr tipe ->
          P.vcat
            [ prettyWire "loopback" name ":" tipe
            , case maybeExpr of
                Just expr ->
                    prettyWire "loopback" name "=" expr
                Nothing ->
                    P.empty
            ]


prettyWire :: (Pretty a) => String -> String -> String -> a -> Doc
prettyWire keyword name op e =
    P.text keyword <+> P.text name <+> P.text op <+> pretty e
