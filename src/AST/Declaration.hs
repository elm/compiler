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

data Declaration' port def var
    = Definition def
    | Datatype String [String] [(String, [T.Type var])]
    | TypeAlias String [String] (T.Type var)
    | Port port
    | Fixity Assoc Int String
      deriving (Show)

data Assoc = L | N | R
    deriving (Eq)

data RawPort
    = PPAnnotation String T.RawType
    | PPDef String Source.Expr
      deriving (Show)

data Port expr var
    = Out String expr (T.Type var)
    | In String (T.Type var)
      deriving (Show)

type SourceDecl    = Declaration' RawPort Source.Def Var.Raw
type ValidDecl     = Declaration' (Port Valid.Expr Var.Raw) Valid.Def Var.Raw
type CanonicalDecl = Declaration' (Port Canonical.Expr Var.Canonical)
                                  Canonical.Def
                                  Var.Canonical

portName :: Port expr var -> String
portName port =
    case port of
      Out name _ _ -> name
      In name _ -> name

instance Show Assoc where
    show assoc =
        case assoc of
          L -> "left"
          N -> "non"
          R -> "right"

instance Binary Assoc where
    get = do n <- getWord8
             return $ case n of
                0 -> L
                1 -> N
                2 -> R
                _ -> error "Error reading valid associativity from serialized string"

    put assoc = putWord8 $ case assoc of { L -> 0 ; N -> 1 ; R -> 2 }

instance (Pretty port, Pretty def, Pretty var, Var.ToString var) =>
    Pretty (Declaration' port def var) where
  pretty decl =
    case decl of
      Definition def -> pretty def

      Datatype tipe tvars ctors ->
          P.hang (P.text "data" <+> P.text tipe <+> P.hsep (map P.text tvars)) 4
               (P.sep $ zipWith join ("=" : repeat "|") ctors)
          where
            join c ctor = P.text c <+> prettyCtor ctor
            prettyCtor (name, tipes) =
                P.hang (P.text name) 2 (P.sep (map T.prettyParens tipes))

      TypeAlias name tvars tipe ->
          P.hang (P.text "type" <+> name' <+> P.equals) 4 (pretty tipe)
          where
            name' = P.text name <+> P.hsep (map P.text tvars)

      Port port -> pretty port

      Fixity assoc prec op -> P.text "infix" <> assoc' <+> P.int prec <+> P.text op
          where
            assoc' = case assoc of
                       L -> P.text "l"
                       N -> P.empty
                       R -> P.text "r"

instance Pretty RawPort where
  pretty port =
    case port of
      PPAnnotation name tipe -> prettyPort name ":"  tipe
      PPDef        name expr -> prettyPort name "=" expr

instance (Pretty expr, Pretty var, Var.ToString var) => Pretty (Port expr var) where
  pretty port =
    case port of
      In name tipe -> prettyPort name ":" tipe
      Out name expr tipe -> P.vcat [ prettyPort name ":" tipe
                                   , prettyPort name "=" expr ]
          

prettyPort :: (Pretty a) => String -> String -> a -> Doc
prettyPort name op e = P.text "port" <+> P.text name <+> P.text op <+> pretty e
