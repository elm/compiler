{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Valid
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
  )
  where


import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Text (Text)

import qualified AST.Binop as Binop
import qualified AST.Expression.Source as Src
import qualified AST.Shader as Shader
import qualified AST.Type as Type
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
    | Lambda [Src.Pattern] Expr
    | Call Expr [Expr]
    | If [(Expr, Expr)] Expr
    | Let [Def] Expr
    | Case Expr [(Src.Pattern, Expr)]
    | Accessor N.Name
    | Access Expr N.Name
    | Update (A.Located N.Name) [(A.Located N.Name, Expr)]
    | Record [(A.Located N.Name, Expr)]
    | Unit
    | Tuple Expr Expr [Expr]
    | Shader Text Text Shader.Shader



-- DEFINITIONS


data Def
    = Define R.Region (A.Located N.Name) [Src.Pattern] Expr (Maybe Type.Raw)
    | Destruct R.Region Src.Pattern Expr



-- MODULE


data Module =
  Module
    { _name     :: N.Name
    , _overview :: A.Located (Maybe B.ByteString)
    , _docs     :: Map.Map N.Name Text
    , _exports  :: Src.Exposing
    , _imports  :: [Src.Import]
    , _decls    :: [A.Located Decl]
    , _unions   :: [Union]
    , _aliases  :: [Alias]
    , _binop    :: [Binop]
    , _effects  :: Effects
    }


data Decl = Decl (A.Located N.Name) [Src.Pattern] Expr (Maybe Type.Raw)
data Union = Union (A.Located N.Name) [A.Located N.Name] [(A.Located N.Name, [Type.Raw])]
data Alias = Alias (A.Located N.Name) [A.Located N.Name] Type.Raw
data Binop = Binop (A.Located N.Name) Binop.Associativity Binop.Precedence N.Name


data Effects
  = NoEffects
  | Ports [Port]
  | Manager R.Region Manager


data Manager
  = Cmd (A.Located N.Name)
  | Sub (A.Located N.Name)
  | Fx (A.Located N.Name) (A.Located N.Name)


data Port = Port (A.Located N.Name) Type.Raw
