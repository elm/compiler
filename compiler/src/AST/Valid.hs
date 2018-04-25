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
import Data.Text (Text)

import qualified AST.Utils.Binop as Binop
import qualified AST.Source as Src
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
    | Var Src.VarType N.Name
    | VarQual Src.VarType N.Name N.Name
    | List [Expr]
    | Op N.Name
    | Negate Expr
    | Binops [(Expr, A.Located N.Name)] Expr
    | Lambda [Src.Pattern] Expr
    | Call Expr [Expr]
    | If [(Expr, Expr)] Expr
    | Let [Def] Expr
    | Case Expr [(Src.Pattern, Expr)]
    | Accessor N.Name
    | Access Expr (A.Located N.Name)
    | Update (A.Located N.Name) [(A.Located N.Name, Expr)]
    | Record [(A.Located N.Name, Expr)]
    | Unit
    | Tuple Expr Expr [Expr]
    | Shader Text Text Shader.Shader



-- DEFINITIONS


data Def
    = Define R.Region (A.Located N.Name) [Src.Pattern] Expr (Maybe Src.Type)
    | Destruct R.Region Src.Pattern Expr



-- MODULE


data Module =
  Module
    { _name     :: N.Name
    , _overview :: Src.Docs
    , _docs     :: Map.Map N.Name Text
    , _exports  :: A.Located Src.Exposing
    , _imports  :: [Src.Import]
    , _decls    :: [A.Located Decl]
    , _unions   :: [Union]
    , _aliases  :: [Alias]
    , _binop    :: [Binop]
    , _effects  :: Effects
    }


data Decl = Decl (A.Located N.Name) [Src.Pattern] Expr (Maybe Src.Type)
data Union = Union R.Region (A.Located N.Name) [A.Located N.Name] [(A.Located N.Name, [Src.Type])]
data Alias = Alias R.Region (A.Located N.Name) [A.Located N.Name] Src.Type
data Binop = Binop N.Name Binop.Associativity Binop.Precedence N.Name


data Effects
  = NoEffects
  | Ports [Port]
  | Manager R.Region Manager


data Manager
  = Cmd (A.Located N.Name)
  | Sub (A.Located N.Name)
  | Fx (A.Located N.Name) (A.Located N.Name)


data Port = Port (A.Located N.Name) Src.Type


defaultModule :: Map.Map N.Name Text -> [Src.Import] -> [A.Located Decl] -> [Union] -> [Alias] -> [Binop] -> Module
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
