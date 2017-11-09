{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Canonical
  ( Expr, Expr_(..)
  , Def(..)
  -- patterns
  , Args(..), Arg(..)
  , Match(..)
  , Pattern, Pattern_(..)
  , PatternCtorArg(..)
  , Destructors
  , Destructor(..)
  -- types
  , Type(..)
  , AliasType(..)
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


import Control.Monad (liftM, liftM2, liftM3, liftM4, replicateM)
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Text (Text)

import qualified AST.Utils.Binop as Binop
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Shader as Shader
import qualified Data.Index as Index
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
  | Chr Text
  | Str Text
  | Int Int
  | Float Double
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
  | Accessor Name
  | Access Expr Name
  | Update Expr (Map.Map Name Expr)
  | Record (Map.Map Name Expr)
  | Unit
  | Tuple Expr Expr (Maybe Expr)
  | Shader Name Name Shader.Shader



-- DEFS


data Def =
  Def
    { _def_name :: A.Located N.Name
    , _def_args :: Args
    , _def_body :: Expr
    , _def_type :: Maybe Type
    }



-- PATTERNS


data Args = Args [Arg] Destructors
data Arg = Arg Index.ZeroBased Pattern

data Match = Match Pattern Destructors


type Pattern = A.Located Pattern_

data Pattern_
  = PAnything
  | PVar N.Name
  | PRecord [N.Name]
  | PAlias Pattern N.Name
  | PUnit
  | PTuple Pattern Pattern (Maybe Pattern)
  | PList [Pattern]
  | PCons Pattern Pattern
  | PChr Text
  | PStr Text
  | PInt Int
  | PCtor
      { _p_home :: ModuleName.Canonical
      , _p_type :: N.Name
      , _p_vars :: [N.Name]
      , _p_name :: N.Name
      , _p_args :: [PatternCtorArg]
      }


data PatternCtorArg =
  PatternCtorArg
    { _index :: Index.ZeroBased
    , _type :: Type
    , _arg :: Pattern
    }


type Destructors = Map.Map N.Name (A.Located Destructor)

data Destructor
  = DIndex Index.ZeroBased Destructor
  | DField N.Name Destructor
  | DRoot Index.ZeroBased



-- TYPES


data Type
  = TLambda Type Type
  | TVar Name
  | TType ModuleName.Canonical Name [Type]
  | TRecord (Map.Map Name Type) (Maybe Type)
  | TUnit
  | TTuple Type Type (Maybe Type)
  | TAlias ModuleName.Canonical Name [(Name, Type)] AliasType


data AliasType
  = Holey Type
  | Filled Type



-- DECLARATIONS


data Decls
  = Declare Decl Decls
  | DeclareRec Decl [Decl] Decls
  | SaveTheEnvironment


data Decl
  = Value N.Name Expr (Maybe Type)
  | Function N.Name Args Expr (Maybe Type)



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


data Alias = Alias [N.Name] Type (Maybe [N.Name])
data Union = Union [N.Name] [(N.Name, [Type])]
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
  = Incoming Type
  | Outgoing Type


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


instance Binary Type where
  put tipe =
    case tipe of
      TLambda a b        -> putWord8 0 >> put a >> put b
      TVar a             -> putWord8 1 >> put a
      TRecord a b        -> putWord8 2 >> put a >> put b
      TUnit              -> putWord8 3
      TTuple a b c       -> putWord8 4 >> put a >> put b >> put c
      TAlias a b c d     -> putWord8 5 >> put a >> put b >> put c >> put d
      TType home name ts ->
        let potentialWord = length ts + 7 in
        if potentialWord <= fromIntegral (maxBound :: Word8) then
          do  putWord8 (fromIntegral potentialWord)
              put home
              put name
              mapM_ put ts
        else
          putWord8 6 >> put home >> put name >> put ts

  get =
    do  word <- getWord8
        case word of
          0 -> liftM2 TLambda get get
          1 -> liftM  TVar get
          2 -> liftM2 TRecord get get
          3 -> return TUnit
          4 -> liftM3 TTuple get get get
          5 -> liftM4 TAlias get get get get
          6 -> liftM3 TType get get get
          n -> liftM3 TType get get (replicateM (fromIntegral (n - 7)) get)


instance Binary AliasType where
  put aliasType =
    case aliasType of
      Holey tipe  -> putWord8 0 >> put tipe
      Filled tipe -> putWord8 1 >> put tipe

  get =
    do  n <- getWord8
        case n of
          0 -> liftM Holey get
          1 -> liftM Filled get
          _ -> error "Error reading a valid type from serialized string"
