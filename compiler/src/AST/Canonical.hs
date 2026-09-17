{-# LANGUAGE ExtendedLiterals, MagicHash, OverloadedStrings #-}
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
  , Exports(..)
  , Export(..)
  , Effects(..)
  , Port(..)
  , Manager(..)
  --
  , eCtorOpts, dCtorOpts
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


import qualified Data.List as List
import qualified Data.Map as Map
import Data.Name (Name)
import GHC.Word (Word16)

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E

import qualified AST.Source as Src
import qualified AST.Prim.Operator as Op
import qualified AST.Utils.Shader as Shader
import qualified Data.Index as Index
import qualified Elm.Float as EF
import qualified Elm.ModuleName as ModuleName
import qualified Elm.String as ES
import qualified Reporting.Annotation as A



-- EXPRESSIONS


type Expr =
  A.Located Expr_


-- CACHE Annotations for type inference
data Expr_
  = VarLocal Name
  | VarTopLevel ModuleName.Canonical Name
  | VarKernel Name Name
  | VarForeign ModuleName.Canonical Name Annotation
  | VarCtor CtorOpts ModuleName.Canonical Name Index.ZeroBased Annotation
  | VarDebug ModuleName.Canonical Name Annotation
  | VarOperator Name ModuleName.Canonical Name Annotation -- CACHE real name for optimization
  | Chr Char
  | Str ES.String
  | Int Int
  | Float EF.Float
  | List [Expr]
  | Negate Expr
  | Binop Name ModuleName.Canonical Name Annotation Expr Expr -- CACHE real name for optimization
  | Lambda [Pattern] Expr
  | Call Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let Def Expr
  | LetRec [Def] Expr
  | LetDestruct Pattern Expr Expr
  | Case Expr [CaseBranch]
  | Accessor Name
  | Access Expr (A.Located Name)
  | Update Name Expr (Map.Map Name FieldUpdate)
  | Record (Map.Map Name Expr)
  | Unit
  | Tuple Expr Expr (Maybe Expr)
  | Shader Shader.Source Shader.Types


data CaseBranch =
  CaseBranch Pattern Expr


data FieldUpdate =
  FieldUpdate A.Region Expr



-- DEFS


data Def
  = Def (A.Located Name) [Pattern] Expr
  | TypedDef (A.Located Name) FreeVars [(Pattern, Type)] Expr Type



-- DECLARATIONS


data Decls
  = Declare Def Decls
  | DeclareRec Def [Def] Decls
  | SaveTheEnvironment



-- PATTERNS


type Pattern =
  A.Located Pattern_


data Pattern_
  = PAnything
  | PVar Name
  | PRecord [Name]
  | PAlias Pattern Name
  | PUnit
  | PTuple Pattern Pattern (Maybe Pattern)
  | PList [Pattern]
  | PCons Pattern Pattern
  | PBool Union Bool
  | PChr Char
  | PStr ES.String
  | PInt Int
  | PCtor
      { _p_home :: ModuleName.Canonical
      , _p_type :: Name
      , _p_union :: Union
      , _p_name :: Name
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
  deriving (Eq)


type FreeVars = Map.Map Name ()


data Type
  = TLambda Type Type
  | TVar Name
  | TType ModuleName.Canonical Name [Type]
  | TRecord (Map.Map Name FieldType) (Maybe Name)
  | TUnit
  | TTuple Type Type (Maybe Type)
  | TAlias ModuleName.Canonical Name [(Name, Type)] AliasType
  deriving (Eq)


data AliasType
  = Holey Type
  | Filled Type
  deriving (Eq)


data FieldType = FieldType {-# UNPACK #-} !Word16 Type
  deriving (Eq)


-- NOTE: The Word16 marks the source order, but it may not be available
-- for every canonical type. For example, if the canonical type is inferred
-- the orders will all be zeros.
--
fieldsToList :: Map.Map Name FieldType -> [(Name, Type)]
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
    , _exports :: Exports
    , _docs    :: Src.Docs
    , _decls   :: Decls
    , _unions  :: Map.Map Name Union
    , _aliases :: Map.Map Name Alias
    , _binops  :: Map.Map Name Binop
    , _effects :: Effects
    }


data Alias = Alias [Name] Type
  deriving (Eq)


data Binop = Binop_ Op.Associativity Op.Precedence Name
  deriving (Eq)


data Union =
  Union
    { _u_vars :: [Name]
    , _u_alts :: [Ctor]
    , _u_numAlts :: Int -- CACHE numAlts for exhaustiveness checking
    , _u_opts :: CtorOpts -- CACHE which optimizations are available
    }
  deriving (Eq)


data CtorOpts
  = Normal
  | Enum
  | Unbox
  deriving (Eq, Ord)


data Ctor = Ctor Name Index.ZeroBased Int [Type] -- CACHE length args
  deriving (Eq)



-- EXPORTS


data Exports
  = ExportEverything A.Region
  | Export (Map.Map Name (A.Located Export))


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
  | Ports (Map.Map Name Port)
  | Manager A.Region A.Region A.Region Manager


data Port
  = Incoming { _freeVars :: FreeVars, _payload :: Type, _func :: Type }
  | Outgoing { _freeVars :: FreeVars, _payload :: Type, _func :: Type }


data Manager
  = Cmd Name
  | Sub Name
  | Fx Name Name



-- BINARY


eCtorOpts :: CtorOpts -> E.Builder
eCtorOpts opts =
  case opts of
    Normal -> E.u8# 0#Word8
    Enum   -> E.u8# 1#Word8
    Unbox  -> E.u8# 2#Word8


dCtorOpts :: D.Decoder CtorOpts
dCtorOpts =
  do  n <- D.u8
      case n of
        0 -> pure Normal
        1 -> pure Enum
        2 -> pure Unbox
        _ -> D.expecting "CtorOpts"


