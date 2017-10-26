{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Expression.Source
  ( Expr, Expr_(..)
  , Decl, Decl_(..)
  , Def(..)
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

import qualified AST.Binop as Binop
import qualified AST.Literal as Literal
import qualified AST.Pattern as Ptrn
import qualified AST.Type as Type
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- EXPRESSIONS


type Expr = A.Located Expr_


data Expr_
  = Literal Literal.Literal
  | Var (Maybe N.Name) N.Name
  | List [Expr]
  | Op N.Name
  | Negate Expr
  | Binops [(Expr, A.Located N.Name)] Expr
  | Lambda Ptrn.Raw Expr
  | Call Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let [A.Located Def] Expr
  | Case Expr [(Ptrn.Raw, Expr)]
  | Accessor N.Name
  | Access Expr N.Name
  | Update (A.Located N.Name) [(A.Located N.Name, Expr)]
  | Record [(A.Located N.Name, Expr)]
  | Unit
  | Tuple Expr Expr [Expr]
  | GLShader Text Text Literal.Shader



-- DEFINITIONS


data Def
  = Annotate Text Type.Raw
  | Define Text [Ptrn.Raw] Expr
  | Destruct Ptrn.Raw Expr



-- DECLARATIONS


type Decl = A.Located Decl_


data Decl_
  = Union (A.Located N.Name) [A.Located N.Name] [(A.Located Text, [Type.Raw])]
  | Alias (A.Located N.Name) [A.Located N.Name] Type.Raw
  | Binop (A.Located N.Name) Binop.Associativity Binop.Precedence N.Name
  | Port (A.Located N.Name) Type.Raw
  | Docs Text
  | Annotation (A.Located N.Name) Type.Raw
  | Definition (A.Located N.Name) [Ptrn.Raw] Expr



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
  | Effects R.Region Manager


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
  = Lower !Text
  | Upper !Text !Privacy
  | Operator !Text


data Privacy
  = Public
  | Private
