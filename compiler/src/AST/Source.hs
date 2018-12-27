{-# OPTIONS_GHC -Wall #-}
module AST.Source
  ( Expr, Expr_(..), VarType(..)
  , Def(..)
  , Pattern, Pattern_(..)
  , Type, Type_(..)
  , Module(..)
  , Import(..)
  , Value(..)
  , Union(..)
  , Alias(..)
  , Binop(..)
  , Port(..)
  , Effects(..)
  , Manager(..)
  , Exposing(..)
  , Exposed(..)
  , Privacy(..)
  )
  where


import Data.Name (Name)
import qualified Data.Utf8 as Utf8

import qualified AST.Utils.Binop as Binop
import qualified AST.Utils.Shader as Shader
import qualified Reporting.Annotation as A



-- EXPRESSIONS


type Expr = A.Located Expr_


data Expr_
  = Chr Utf8.String
  | Str Utf8.String
  | Int Int
  | Float (Utf8.Under256 Float)
  | Var VarType Name
  | VarQual VarType Name Name
  | List [Expr]
  | Op Name
  | Negate Expr
  | Binops [(Expr, A.Located Name)] Expr
  | Lambda [Pattern] Expr
  | Call Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let [A.Located Def] Expr
  | Case Expr [(Pattern, Expr)]
  | Accessor Name
  | Access Expr (A.Located Name)
  | Update (A.Located Name) [(A.Located Name, Expr)]
  | Record [(A.Located Name, Expr)]
  | Unit
  | Tuple Expr Expr [Expr]
  | Shader Shader.Source Shader.Types


data VarType = LowVar | CapVar



-- DEFINITIONS


data Def
  = Define (A.Located Name) [Pattern] Expr (Maybe Type)
  | Destruct Pattern Expr



-- PATTERN


type Pattern = A.Located Pattern_


data Pattern_
  = PAnything
  | PVar Name
  | PRecord [A.Located Name]
  | PAlias Pattern (A.Located Name)
  | PUnit
  | PTuple Pattern Pattern [Pattern]
  | PCtor A.Region Name [Pattern]
  | PCtorQual A.Region Name Name [Pattern]
  | PList [Pattern]
  | PCons Pattern Pattern
  | PChr Utf8.String
  | PStr Utf8.String
  | PInt Int



-- TYPE


type Type =
    A.Located Type_


data Type_
  = TLambda Type Type
  | TVar Name
  | TType A.Region Name [Type]
  | TTypeQual A.Region Name Name [Type]
  | TRecord [(A.Located Name, Type)] (Maybe (A.Located Name))
  | TUnit
  | TTuple Type Type [Type]



-- MODULE


data Module =
  Module
    { _name    :: Name
    , _exports :: A.Located Exposing
    , _imports :: [Import]
    , _values  :: [A.Located Value]
    , _unions  :: [A.Located Union]
    , _aliases :: [A.Located Alias]
    , _binops  :: [A.Located Binop]
    , _effects :: Effects
    }


data Import =
  Import
    { _import :: A.Located Name
    , _alias :: Maybe Name
    , _exposing :: Exposing
    }


data Value = Value (A.Located Name) [Pattern] Expr (Maybe Type)
data Union = Union (A.Located Name) [A.Located Name] [(A.Located Name, [Type])]
data Alias = Alias (A.Located Name) [A.Located Name] Type
data Binop = Binop Name Binop.Associativity Binop.Precedence Name
data Port = Port (A.Located Name) Type


data Effects
  = NoEffects
  | Ports [Port]
  | Manager A.Region Manager


data Manager
  = Cmd (A.Located Name)
  | Sub (A.Located Name)
  | Fx (A.Located Name) (A.Located Name)



-- EXPOSING


data Exposing
  = Open
  | Explicit [A.Located Exposed]


data Exposed
  = Lower Name
  | Upper Name Privacy
  | Operator Name


data Privacy
  = Public
  | Private
