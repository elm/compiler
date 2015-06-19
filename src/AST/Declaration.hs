{-# OPTIONS_GHC -Wall #-}
module AST.Declaration where

import Data.Binary

import qualified AST.Expression.General as General
import qualified AST.Expression.Source as Source
import qualified AST.Expression.Valid as Valid
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Type as Type
import qualified Reporting.Annotation as A


-- DECLARATIONS

data Declaration' port def tipe expr
    = Definition def
    | Datatype String [String] [(String, [tipe])]
    | TypeAlias String [String] tipe
    | Port port
    | Fixity Assoc Int String


-- INFIX STUFF

data Assoc = L | N | R
    deriving (Eq)


assocToString :: Assoc -> String
assocToString assoc =
    case assoc of
      L -> "left"
      N -> "non"
      R -> "right"


-- DECLARATION PHASES

type SourceDecl' =
  Declaration' SourcePort Source.Def Type.Raw Source.Expr


data SourceDecl
    = Comment String
    | Decl (A.Located SourceDecl')


type ValidDecl =
  A.Commented (Declaration' ValidPort Valid.Def Type.Raw Valid.Expr)


type CanonicalDecl =
  A.Commented (Declaration' CanonicalPort Canonical.Def Type.Canonical Canonical.Expr)


-- PORTS

data SourcePort
    = PortAnnotation String Type.Raw
    | PortDefinition String Source.Expr


data ValidPort
    = In String Type.Raw
    | Out String Valid.Expr Type.Raw


newtype CanonicalPort
    = CanonicalPort (General.PortImpl Canonical.Expr Type.Canonical)


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
