{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Canonical
  ( Expr, Expr_(..)
  , CaseBranch(..)
  -- definitions
  , Def(..)
  , Arg(..)
  , TypedArg(..)
  , Decls(..)
  -- patterns
  , Pattern, Pattern_(..)
  , CtorAlts(..)
  , CtorAlt(..)
  , toAlts
  , ctorsToAlts
  , PatternCtorArg(..)
  -- destructors
  , Destructors
  , DestructorInfo(..)
  , Destructor(..)
  -- types
  , Annotation(..)
  , Type(..)
  , AliasType(..)
  -- modules
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

{- Creating a canonical AST means finding the home module for all variables.
So if you have L.map, you need to figure out that it is from the elm-lang/core
package in the List module.

In later phases (e.g. type inference, exhaustiveness checking, optimization)
you need to look up additional info from these modules. What is the type?
What are the alternative type constructors? These lookups can be quite costly,
especially in type inference. To reduce costs the canonicalization phase
caches info needed in later phases. This means we no longer build large
dictionaries of metadata with O(log(n)) lookups in those phases. Instead
there is an O(1) read of an existing field! I have tried to mark all
cached data with comments like:

-- CACHE for exhaustiveness
-- CACHE for inference

So it is clear why the data is kept around.
-}

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
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- EXPRESSIONS


type Expr =
  A.Located Expr_


data Expr_
  = VarLocal N.Name
  | VarTopLevel ModuleName.Canonical N.Name
  | VarKernel N.Name N.Name
  | VarForeign ModuleName.Canonical N.Name Annotation
  -- CACHE Annotation for inference
  | VarOperator N.Name ModuleName.Canonical N.Name Annotation
  -- CACHE real name for optimization
  -- CACHE Annotation for inference
  | Chr Text
  | Str Text
  | Int Int
  | Float Double
  | List [Expr]
  | Negate Expr
  | Binop N.Name ModuleName.Canonical N.Name Annotation Expr Expr
  -- CACHE real name for optimization
  -- CACHE Annotation for inference
  | Lambda [Arg] Destructors Expr
  | Call Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let Def Expr
  | LetRec [Def] Expr
  | LetDestruct Pattern Destructors Expr Expr
  | Case Expr [CaseBranch]
  | Accessor N.Name
  | Access Expr N.Name
  | Update Expr (Map.Map N.Name Expr)
  | Record (Map.Map N.Name Expr)
  | Unit
  | Tuple Expr Expr (Maybe Expr)
  | Shader N.Name N.Name Shader.Shader


data CaseBranch =
  CaseBranch Pattern Destructors Expr



-- DEFS


data Def
  = Def (A.Located N.Name) [Arg] Destructors Expr
  | TypedDef (A.Located N.Name) FreeVars [TypedArg] Destructors Expr Type


data Arg =
  Arg Index.ZeroBased Pattern


data TypedArg =
  TypedArg Index.ZeroBased Type Pattern



-- DECLARATIONS


data Decls
  = Declare Def Decls
  | DeclareRec [Def] Decls
  | SaveTheEnvironment



-- PATTERNS


type Pattern =
  A.Located Pattern_


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
      , _p_alts :: CtorAlts
      , _p_name :: N.Name
      , _p_args :: [PatternCtorArg]
      }
      -- CACHE _p_home, _p_type, and _p_vars for type inference
      -- CACHE _p_alts for exhaustiveness checker


data PatternCtorArg =
  PatternCtorArg
    { _index :: Index.ZeroBased -- CACHE for destructors/errors
    , _type :: Type             -- CACHE for type inference
    , _arg :: Pattern
    }


data CtorAlts =
  CtorAlts
    { _num :: Int -- CACHE (length _alts) for exhaustiveness
    , _alts :: [CtorAlt]
    }
  deriving (Eq)


data CtorAlt =
  CtorAlt
    { _ctor :: N.Name
    , _arity :: Int
    }
  deriving (Eq)



-- PATTERN CTOR HELPERS


toAlts :: [CtorAlt] -> CtorAlts
toAlts alts =
  CtorAlts (length alts) alts


ctorsToAlts :: [(N.Name, [Type])] -> CtorAlts
ctorsToAlts ctors =
  CtorAlts (length ctors) (map toAlt ctors)


toAlt :: (N.Name, [Type]) -> CtorAlt
toAlt (ctor, argTypes) =
  CtorAlt ctor (length argTypes)



-- DESTRUCTORS


type Destructors =
  Map.Map N.Name DestructorInfo


data DestructorInfo =
  DestructorInfo
    { _destructor :: Destructor
    , _direct_uses :: Int  -- CACHE for inlining
    , _delayed_uses :: Int -- CACHE for inlining
    }


data Destructor
  = DIndex Index.ZeroBased Destructor
  | DField N.Name Destructor
  | DRoot Index.ZeroBased



-- TYPES


data Annotation = Forall FreeVars Type


type FreeVars = Map.Map N.Name ()


data Type
  = TLambda Type Type
  | TVar N.Name
  | TType ModuleName.Canonical N.Name [Type]
  | TRecord (Map.Map N.Name Type) (Maybe Type)
  | TUnit
  | TTuple Type Type (Maybe Type)
  | TAlias ModuleName.Canonical N.Name [(N.Name, Type)] AliasType


data AliasType
  = Holey Type
  | Filled Type



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
  = Incoming FreeVars Type
  | Outgoing FreeVars Type


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


instance Binary Annotation where
  get = liftM2 Forall get get
  put (Forall a b) = put a >> put b


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
