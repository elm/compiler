{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Canonical
  ( Expr, Expr_(..)
  , CaseBranch(..)
  , FieldUpdate(..)
  , CtorOpts(..)
  -- definitions
  , Def(..)
  , Decls(..)
  -- patterns
  , Pattern, Pattern_(..)
  , PatternCtorArg(..)
  -- types
  , Annotation(..)
  , Type(..)
  , AliasType(..)
  , FieldType(..)
  , fieldsToList
  -- modules
  , Module(..)
  , Alias(..)
  , Binop(..)
  , Union(..)
  , Ctor(..)
  , Docs(..)
  , Exports(..)
  , Export(..)
  , Effects(..)
  , Port(..)
  , Manager(..)
  )
  where

{- Creating a canonical AST means finding the home module for all variables.
So if you have L.map, you need to figure out that it is from the elm/core
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
import qualified Data.List as List
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


-- CACHE Annotations for type inference
data Expr_
  = VarLocal N.Name
  | VarTopLevel ModuleName.Canonical N.Name
  | VarKernel N.Name N.Name
  | VarForeign ModuleName.Canonical N.Name Annotation
  | VarCtor CtorOpts ModuleName.Canonical N.Name Index.ZeroBased Annotation
  | VarDebug ModuleName.Canonical N.Name Annotation
  | VarOperator N.Name ModuleName.Canonical N.Name Annotation -- CACHE real name for optimization
  | Chr Text
  | Str Text
  | Int Int
  | Float Double
  | List [Expr]
  | Negate Expr
  | Binop N.Name ModuleName.Canonical N.Name Annotation Expr Expr -- CACHE real name for optimization
  | Lambda [Pattern] Expr
  | Call Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let Def Expr
  | LetRec [Def] Expr
  | LetDestruct Pattern Expr Expr
  | Case Expr [CaseBranch]
  | Accessor N.Name
  | Access Expr (A.Located N.Name)
  | Update N.Name Expr (Map.Map N.Name FieldUpdate)
  | Record (Map.Map N.Name Expr)
  | Unit
  | Tuple Expr Expr (Maybe Expr)
  | Shader Text Text Shader.Shader


data CaseBranch =
  CaseBranch Pattern Expr


data FieldUpdate =
  FieldUpdate R.Region Expr



-- DEFS


data Def
  = Def (A.Located N.Name) [Pattern] Expr
  | TypedDef (A.Located N.Name) FreeVars [(Pattern, Type)] Expr Type



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
  | PBool Union Bool
  | PChr Text
  | PStr Text
  | PInt Int
  | PCtor
      { _p_home :: ModuleName.Canonical
      , _p_type :: N.Name
      , _p_union :: Union
      , _p_name :: N.Name
      , _p_index :: Index.ZeroBased
      , _p_args :: [PatternCtorArg]
      }
      -- CACHE _p_home, _p_type, and _p_vars for type inference
      -- CACHE _p_index to replace _p_name in PROD code gen
      -- CACHE _p_opts to allocate less in PROD code gen
      -- CACHE _p_alts and _p_numAlts for exhaustiveness checker


data PatternCtorArg =
  PatternCtorArg
    { _index :: Index.ZeroBased -- CACHE for destructors/errors
    , _type :: Type             -- CACHE for type inference
    , _arg :: Pattern
    }



-- TYPES


data Annotation = Forall FreeVars Type


type FreeVars = Map.Map N.Name ()


data Type
  = TLambda Type Type
  | TVar N.Name
  | TType ModuleName.Canonical N.Name [Type]
  | TRecord (Map.Map N.Name FieldType) (Maybe N.Name)
  | TUnit
  | TTuple Type Type (Maybe Type)
  | TAlias ModuleName.Canonical N.Name [(N.Name, Type)] AliasType


data AliasType
  = Holey Type
  | Filled Type


data FieldType =
  FieldType {-# UNPACK #-} !Word16 Type


-- NOTE: The Word16 marks the source order, but it may not be available
-- for every canonical type. For example, if the canonical type is inferred
-- the orders will all be zeros.
--
fieldsToList :: Map.Map N.Name FieldType -> [(N.Name, Type)]
fieldsToList fields =
  let
    getIndex (_, FieldType index _) =
      index

    dropIndex (name, FieldType _ tipe) =
      (name, tipe)
  in
  map dropIndex (List.sortOn getIndex (Map.toList fields))



-- MODULES


data Module =
  Module
    { _name    :: ModuleName.Canonical
    , _docs    :: Docs
    , _exports :: Exports
    , _decls   :: Decls
    , _unions  :: Map.Map N.Name Union
    , _aliases :: Map.Map N.Name Alias
    , _binops  :: Map.Map N.Name Binop
    , _effects :: Effects
    }


data Alias = Alias [N.Name] Type
data Binop = Binop_ Binop.Associativity Binop.Precedence N.Name


data Union =
  Union
    { _u_vars :: [N.Name]
    , _u_alts :: [Ctor]
    , _u_numAlts :: Int -- CACHE numAlts for exhaustiveness checking
    , _u_opts :: CtorOpts -- CACHE which optimizations are available
    }


data CtorOpts
  = Normal
  | Enum
  | Unbox
  deriving (Eq, Ord)


data Ctor =
  Ctor N.Name Index.ZeroBased Int [Type] -- CACHE length args



-- DOCS


data Docs
  = NoDocs R.Region
  | YesDocs
      { _region :: R.Region
      , _overview :: B.ByteString
      , _comments :: Map.Map N.Name Text
      }



-- EXPORTS


data Exports
  = ExportEverything R.Region
  | Export (Map.Map N.Name (A.Located Export))


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
  = Incoming { _freeVars :: FreeVars, _payload :: Type, _func :: Type }
  | Outgoing { _freeVars :: FreeVars, _payload :: Type, _func :: Type }


data Manager
  = Cmd N.Name
  | Sub N.Name
  | Fx N.Name N.Name



-- BINARY


instance Binary Alias where
  get = liftM2 Alias get get
  put (Alias a b) = put a >> put b


instance Binary Union where
  put (Union a b c d) = put a >> put b >> put c >> put d
  get = liftM4 Union get get get get


instance Binary Ctor where
  get = liftM4 Ctor get get get get
  put (Ctor a b c d) = put a >> put b >> put c >> put d


instance Binary CtorOpts where
  put opts =
    case opts of
      Normal -> putWord8 0
      Enum   -> putWord8 1
      Unbox  -> putWord8 2

  get =
    do  n <- getWord8
        case n of
          0 -> return Normal
          1 -> return Enum
          2 -> return Unbox
          _ -> error "binary encoding of CtorOpts was corrupted"


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
          _ -> error "binary encoding of AliasType was corrupted"


instance Binary FieldType where
  get = liftM2 FieldType get get
  put (FieldType a b) = put a >> put b
