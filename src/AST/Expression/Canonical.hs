{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Expression.Canonical
  ( Expr, Expr_(..)
  , Def(..)
  -- patterns
  , Args(..), Arg(..)
  , Match(..)
  , Pattern, Pattern_(..)
  , Destructors
  , Destructor(..)
  -- decls
  , Decls(..)
  , Decl(..)
  , Module(..)
  , Alias(..)
  , Union(..)
  , Binop(..)
  , Docs(..)
  , Exports(..)
  , Export(..)
  , Effects(..)
  , Port(..)
  , Manager(..)
  )
  where


import Control.Monad (liftM2, liftM3)
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Text (Text)

import qualified AST.Binop as Binop
import qualified AST.Literal as Literal
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified Elm.Name as N
import Elm.Name (Name)
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- EXPRESSIONS


type Expr =
  A.Located Expr_


data Expr_
  = VarLocal Name
  | VarTopLevel ModuleName.Canonical Name
  | VarKernel Name Name
  | VarForeign ModuleName.Canonical Name
  | VarOperator Name ModuleName.Canonical Name
  | Literal Literal.Literal
  | List [Expr]
  | Negate Expr
  | Binop Name ModuleName.Canonical Name Expr Expr
  | Lambda Args Expr
  | Call Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let Def Expr
  | LetRec [Def] Expr
  | LetDestruct Match Expr Expr
  | Case Expr [(Match, Expr)]
  | CtorAccess Expr Int
  | Accessor Name
  | Access Expr Name
  | Update Expr (Map.Map Name Expr)
  | Record (Map.Map Name Expr)
  | Unit
  | Tuple Expr Expr (Maybe Expr)
  | GLShader Name Name Literal.Shader



-- DEFS


data Def =
  Def
    { _def_name :: A.Located N.Name
    , _def_args :: Args
    , _def_body :: Expr
    , _def_type :: Maybe Type.Canonical
    }



-- PATTERNS


data Args = Args [Arg] Destructors
data Arg = Arg Int Pattern

data Match = Match Pattern Destructors


type Pattern = A.Located Pattern_

data Pattern_
  = PAnything
  | PVar N.Name
  | PRecord [N.Name]
  | PAlias Pattern N.Name
  | PUnit
  | PTuple Pattern Pattern (Maybe Pattern)
  | PCtor ModuleName.Canonical N.Name [Pattern]
  | PList [Pattern]
  | PCons Pattern Pattern
  | PLiteral Literal.Literal


type Destructors = Map.Map N.Name (A.Located Destructor)

data Destructor
  = DIndex Int Destructor
  | DField N.Name Destructor
  | DRoot Int



-- DECLARATIONS


data Decls
  = Declare Decl Decls
  | DeclareRec Decl [Decl] Decls
  | SaveTheEnvironment


data Decl
  = Value N.Name Expr (Maybe Type.Canonical)
  | Function N.Name Args Expr (Maybe Type.Canonical)



-- MODULES


data Module =
  Module
    { _name    :: ModuleName.Canonical
    , _docs    :: A.Located (Maybe Docs)
    , _exports :: Exports
    , _decls   :: Decls
    , _unions  :: Map.Map N.Name Union
    , _aliases :: Map.Map N.Name Alias
    , _binops  :: Map.Map N.Name Binop
    , _effects :: Effects
    }


data Alias = Alias [N.Name] Type.Canonical (Maybe [N.Name])
data Union = Union [N.Name] [(N.Name, [Type.Canonical])]
data Binop = Binop_ Binop.Associativity Binop.Precedence N.Name



-- DOCS


data Docs =
  Docs
    { _overview :: B.ByteString
    , _comments :: Map.Map N.Name Text
    }



-- EXPORTS


data Exports
  = ExportEverything
  | Export (Map.Map N.Name Export)


data Export
  = ExportValue
  | ExportBinop
  | ExportAlias
  | ExportUnionOpen
  | ExportUnionClosed
  | ExportPort



-- EFFECTS


data Effects
  = NoEffects
  | Ports (Map.Map N.Name Port)
  | Manager R.Region R.Region R.Region Manager


data Port
  = Incoming Type.Canonical
  | Outgoing Type.Canonical


data Manager
  = Cmd N.Name
  | Sub N.Name
  | Fx N.Name N.Name



-- BINARY


instance Binary Alias where
  get = liftM3 Alias get get get
  put (Alias a b c) = put a >> put b >> put c


instance Binary Union where
  get = liftM2 Union get get
  put (Union a b) = put a >> put b


instance Binary Binop where
  get = liftM3 Binop_ get get get
  put (Binop_ a b c) = put a >> put b >> put c
