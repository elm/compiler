{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Source
  ( Expr, Expr_(..)
  , Decl, Decl_(..)
  , Def(..)
  , Pattern, Pattern_(..)
  , Type, Type_(..)
  , Module(..)
  , Import(..)
  , Effects(..)
  , Manager(..)
  , defaultModule
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
  | Var (Maybe N.Name) N.Name
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
  | Access Expr N.Name
  | Update (A.Located N.Name) [(A.Located N.Name, Expr)]
  | Record [(A.Located N.Name, Expr)]
  | Unit
  | Tuple Expr Expr [Expr]
  | Shader Text Text Shader.Shader



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
  | PCtor R.Region (Maybe N.Name) N.Name [Pattern]
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
  | TType R.Region (Maybe N.Name) N.Name [Type]
  | TRecord [(A.Located N.Name, Type)] (Maybe Type)
  | TUnit
  | TTuple Type Type [Type]



-- DECLARATIONS


type Decl = A.Located Decl_


data Decl_
  = Union (A.Located N.Name) [A.Located N.Name] [(A.Located N.Name, [Type])]
  | Alias (A.Located N.Name) [A.Located N.Name] Type
  | Binop (A.Located N.Name) Binop.Associativity Binop.Precedence N.Name
  | Port (A.Located N.Name) Type
  | Docs Text
  | Annotation (A.Located N.Name) Type
  | Definition (A.Located N.Name) [Pattern] Expr



-- MODULE


data Module decls =
  Module
    { _name :: N.Name
    , _effects :: Effects
    , _docs :: A.Located (Maybe B.ByteString)
    , _exports :: Exposing
    , _imports :: [Import]
    , _decls :: decls
    }


data Import =
  Import
    { _import :: A.Located N.Name
    , _alias :: Maybe N.Name
    , _exposing :: Exposing
    }


data Effects
  = NoEffects
  | Ports R.Region
  | Manager R.Region Manager


data Manager
  = Cmd (A.Located N.Name)
  | Sub (A.Located N.Name)
  | Fx (A.Located N.Name) (A.Located N.Name)


defaultModule :: [Import] -> decls -> Module decls
defaultModule imports decls =
  let zero = R.Position 1 1 in
  Module
    { _name = "Main"
    , _effects = NoEffects
    , _docs = A.at zero zero Nothing
    , _exports = Open
    , _imports = imports
    , _decls = decls
    }



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
