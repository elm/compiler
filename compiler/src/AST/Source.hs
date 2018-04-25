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
import Data.Text (Text)

import qualified AST.Utils.Binop as Binop
import qualified AST.Utils.Shader as Shader
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- EXPRESSIONS


type Expr = A.Located Expr_


data Expr_
  = Chr Text
  | Str Text
  | Int Int
  | Float Double
  | Var VarType N.Name
  | VarQual VarType N.Name N.Name
  | List [Expr]
  | Op N.Name
  | Negate Expr
  | Binops [(Expr, A.Located N.Name)] Expr
  | Lambda [Pattern] Expr
  | Call Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let [A.Located Def] Expr
  | Case Expr [(Pattern, Expr)]
  | Accessor N.Name
  | Access Expr (A.Located N.Name)
  | Update (A.Located N.Name) [(A.Located N.Name, Expr)]
  | Record [(A.Located N.Name, Expr)]
  | Unit
  | Tuple Expr Expr [Expr]
  | Shader Text Text Shader.Shader


data VarType = Value | Ctor



-- DEFINITIONS


data Def
  = Annotate N.Name Type
  | Define (A.Located N.Name) [Pattern] Expr
  | Destruct Pattern Expr



-- PATTERN


type Pattern = A.Located Pattern_


data Pattern_
  = PAnything
  | PVar N.Name
  | PRecord [A.Located N.Name]
  | PAlias Pattern (A.Located N.Name)
  | PUnit
  | PTuple Pattern Pattern [Pattern]
  | PCtor R.Region N.Name [Pattern]
  | PCtorQual R.Region N.Name N.Name [Pattern]
  | PList [Pattern]
  | PCons Pattern Pattern
  | PChr Text
  | PStr Text
  | PInt Int



-- TYPE


type Type =
    A.Located Type_


data Type_
  = TLambda Type Type
  | TVar N.Name
  | TType R.Region N.Name [Type]
  | TTypeQual R.Region N.Name N.Name [Type]
  | TRecord [(A.Located N.Name, Type)] (Maybe (A.Located N.Name))
  | TUnit
  | TTuple Type Type [Type]



-- DECLARATIONS


type Decl = A.Located Decl_


data Decl_
  = Union (A.Located N.Name) [A.Located N.Name] [(A.Located N.Name, [Type])]
  | Alias (A.Located N.Name) [A.Located N.Name] Type
  | Binop N.Name Binop.Associativity Binop.Precedence N.Name
  | Port (A.Located N.Name) Type
  | Docs Text
  | Annotation (A.Located N.Name) Type
  | Definition (A.Located N.Name) [Pattern] Expr



-- MODULE


data Module decls =
  Module (Maybe Header) [Import] decls


data Header
  = Header
      { _name :: N.Name
      , _effects :: Effects
      , _exports :: A.Located Exposing
      , _docs :: Docs
      }


data Import =
  Import
    { _import :: A.Located N.Name
    , _alias :: Maybe N.Name
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
  = Cmd (A.Located N.Name)
  | Sub (A.Located N.Name)
  | Fx (A.Located N.Name) (A.Located N.Name)



-- EXPOSING


data Exposing
  = Open
  | Explicit ![A.Located Exposed]


data Exposed
  = Lower !N.Name
  | Upper !N.Name !Privacy
  | Operator !N.Name


data Privacy
  = Public
  | Private
