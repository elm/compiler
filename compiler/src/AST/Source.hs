{-# OPTIONS_GHC -Wall #-}
module AST.Source
  ( Expr, Expr_(..), VarType(..)
  , Decl, Decl_(..)
  , Def(..)
  , Pattern, Pattern_(..)
  , Type, Type_(..)
  , Module(..)
  , Header(..)
  , Docs(..)
  , Import(..)
  , Effects(..)
  , Manager(..)
  , Exposing(..)
  , Exposed(..)
  , Privacy(..)
  )
  where


import qualified Data.ByteString as B
import Data.Name (Name)
import Data.Text (Text)
import Data.Utf8 (Utf8)

import qualified AST.Utils.Binop as Binop
import qualified AST.Utils.Shader as Shader
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- EXPRESSIONS


type Expr = A.Located Expr_


data Expr_
  = Chr Utf8
  | Str Utf8
  | Int Int
  | Float Double
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
  | Shader Text Text Shader.Shader


data VarType = Value | Ctor



-- DEFINITIONS


data Def
  = Annotate Name Type
  | Define (A.Located Name) [Pattern] Expr
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
  | PCtor R.Region Name [Pattern]
  | PCtorQual R.Region Name Name [Pattern]
  | PList [Pattern]
  | PCons Pattern Pattern
  | PChr Utf8
  | PStr Utf8
  | PInt Int



-- TYPE


type Type =
    A.Located Type_


data Type_
  = TLambda Type Type
  | TVar Name
  | TType R.Region Name [Type]
  | TTypeQual R.Region Name Name [Type]
  | TRecord [(A.Located Name, Type)] (Maybe (A.Located Name))
  | TUnit
  | TTuple Type Type [Type]



-- DECLARATIONS


type Decl = A.Located Decl_


data Decl_
  = Union (A.Located Name) [A.Located Name] [(A.Located Name, [Type])]
  | Alias (A.Located Name) [A.Located Name] Type
  | Binop Name Binop.Associativity Binop.Precedence Name
  | Port (A.Located Name) Type
  | Docs Text
  | Annotation (A.Located Name) Type
  | Definition (A.Located Name) [Pattern] Expr



-- MODULE


data Module decls =
  Module (Maybe Header) [Import] decls


data Header
  = Header
      { _name :: Name
      , _effects :: Effects
      , _exports :: A.Located Exposing
      , _docs :: Docs
      }


data Import =
  Import
    { _import :: A.Located Name
    , _alias :: Maybe Name
    , _exposing :: Exposing
    }


data Docs
  = NoDocs R.Region
  | YesDocs R.Region B.ByteString


data Effects
  = NoEffects
  | Ports R.Region
  | Manager R.Region Manager


data Manager
  = Cmd (A.Located Name)
  | Sub (A.Located Name)
  | Fx (A.Located Name) (A.Located Name)



-- EXPOSING


data Exposing
  = Open
  | Explicit ![A.Located Exposed]


data Exposed
  = Lower !Name
  | Upper !Name !Privacy
  | Operator !Name


data Privacy
  = Public
  | Private
