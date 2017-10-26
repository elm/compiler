{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Expression.Canonical
  ( Expr
  , Expr_(..)
  , Def(..)
  , Decl(..)
  , Module(..)
  , Alias(..)
  , Union(..)
  , Binop(..)
  , Effects(..)
  , Port(..)
  , Docs(..)
  )
  where


import Control.Monad (liftM2, liftM3)
import Data.Binary
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import Data.Text (Text)

import qualified AST.Binop as Binop
import qualified AST.Literal as Literal
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as Ptrn
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Elm.Name as N
import Elm.Name (Name)
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- EXPRESSIONS


type Expr =
  A.Annotated R.Region Expr_


data Expr_
  = VarLocal Name
  | VarTopLevel ModuleName.Canonical Name
  | VarKernel Name Name
  | VarForeign ModuleName.Canonical Name
  | VarOperator ModuleName.Canonical Name Name
  | Literal Literal.Literal
  | List [Expr]
  | Binop Name ModuleName.Canonical Name Expr Expr
  | Lambda Ptrn.Canonical Expr
  | Call Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let [Graph.SCC Def] Expr
  | Case Expr [(Ptrn.Canonical, Expr)]
  | CtorAccess Expr Int
  | Accessor Name
  | Access Expr Name
  | Update Expr [(Name, Expr)]
  | Record [(Name, Expr)]
  | Unit
  | Tuple Expr Expr [Expr]
  | GLShader Name Name Literal.Shader



-- DEFS


data Def =
  Def
    { _let_name :: Text
    , _let_args :: [A.Located Ptrn.Canonical]
    , _let_body :: Expr
    , _let_type :: Maybe (A.Located Type.Canonical)
    }



-- DECLARATIONS


data Decl =
  Decl
    { _top_name :: Text
    , _top_args :: [A.Located Ptrn.Canonical]
    , _top_body :: Expr
    , _top_type :: Maybe (A.Located Type.Canonical)
    , _top_deps :: [Var.Global]
    }



-- MODULES


data Module phase =
  Module
    { _name    :: ModuleName.Canonical
    , _docs    :: A.Located (Maybe Docs)
    , _imports :: [N.Name]
    , _decls   :: [Graph.SCC Decl]
    , _unions  :: Map.Map N.Name Union
    , _aliases :: Map.Map N.Name Alias
    , _binops  :: Map.Map N.Name Binop
    , _effects :: Effects
    }


data Alias = Alias [N.Name] Type.Canonical
data Union = Union [N.Name] [(N.Name, [Type.Canonical])]
data Binop = Binop_ Binop.Associativity Binop.Precedence N.Name

data Effects
  = NoEffects
  | Ports [Port]
  | Cmd
  | Sub
  | Fx


data Port = Port R.Region N.Name Type.Canonical



-- DOCS


data Docs =
  Docs
    { _overview :: Text
    , _comments :: Map.Map N.Name Text
    }



-- BINARY


instance Binary Alias where
  get = liftM2 Alias get get
  put (Alias a b) = put a >> put b


instance Binary Union where
  get = liftM2 Union get get
  put (Union a b) = put a >> put b


instance Binary Binop where
  get = liftM3 Binop_ get get get
  put (Binop_ a b c) = put a >> put b >> put c
