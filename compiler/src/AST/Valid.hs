{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Valid
  ( Expr, Expr_(..)
  , Def(..)
  , Module(..)
  , Decl(..)
  , Union(..)
  , Alias(..)
  , Binop(..)
  , Effects(..)
  , Port(..)
  , Manager(..)
  , defaultModule
  )
  where


import qualified Data.Map as Map
import Data.Name (Name)
import Data.Text (Text)
import Data.Utf8 (Utf8)

import qualified AST.Utils.Binop as Binop
import qualified AST.Source as Src
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
    | Var Src.VarType Name
    | VarQual Src.VarType Name Name
    | List [Expr]
    | Op Name
    | Negate Expr
    | Binops [(Expr, A.Located Name)] Expr
    | Lambda [Src.Pattern] Expr
    | Call Expr [Expr]
    | If [(Expr, Expr)] Expr
    | Let [Def] Expr
    | Case Expr [(Src.Pattern, Expr)]
    | Accessor Name
    | Access Expr (A.Located Name)
    | Update (A.Located Name) [(A.Located Name, Expr)]
    | Record [(A.Located Name, Expr)]
    | Unit
    | Tuple Expr Expr [Expr]
    | Shader Text Text Shader.Shader



-- DEFINITIONS


data Def
    = Define R.Region (A.Located Name) [Src.Pattern] Expr (Maybe Src.Type)
    | Destruct R.Region Src.Pattern Expr



-- MODULE


data Module =
  Module
    { _name     :: Name
    , _overview :: Src.Docs
    , _docs     :: Map.Map Name Text
    , _exports  :: A.Located Src.Exposing
    , _imports  :: [Src.Import]
    , _decls    :: [A.Located Decl]
    , _unions   :: [Union]
    , _aliases  :: [Alias]
    , _binop    :: [Binop]
    , _effects  :: Effects
    }


data Decl = Decl (A.Located Name) [Src.Pattern] Expr (Maybe Src.Type)
data Union = Union R.Region (A.Located Name) [A.Located Name] [(A.Located Name, [Src.Type])]
data Alias = Alias R.Region (A.Located Name) [A.Located Name] Src.Type
data Binop = Binop Name Binop.Associativity Binop.Precedence Name


data Effects
  = NoEffects
  | Ports [Port]
  | Manager R.Region Manager


data Manager
  = Cmd (A.Located Name)
  | Sub (A.Located Name)
  | Fx (A.Located Name) (A.Located Name)


data Port = Port (A.Located Name) Src.Type


defaultModule :: Map.Map Name Text -> [Src.Import] -> [A.Located Decl] -> [Union] -> [Alias] -> [Binop] -> Module
defaultModule docs imports decls unions aliases binop =
  Module
    { _name     = "Main"
    , _overview = Src.NoDocs R.one
    , _docs     = docs
    , _exports  = A.At R.one Src.Open
    , _imports  = imports
    , _decls    = decls
    , _unions   = unions
    , _aliases  = aliases
    , _binop    = binop
    , _effects  = NoEffects
    }
