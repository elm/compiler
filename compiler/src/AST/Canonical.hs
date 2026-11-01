{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module AST.Canonical
  ( Decls(..)
  , Decl(..)
  , Def(..)
  --
  , Expr
  , Expr_(..)
  , Branch(..)
  , Update(..)
  --
  , Value(..)
  , Func(..)
  --
  , Pattern
  , Pattern_(..)
  , PArg(..)
  , PType(..)
  --
  , Annotation(..)
  , Type
  , Type_(..)
  , toIAnnotation
  , toIType
  --
  , Module(..)
  , TypeDecl(..)
  , Binop(..)
  , Exports(..)
  , Style(..)
  , Port(..)
  , Manager(..)
  , EType(..)
  , EFFECT(..)
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


import qualified Data.Dups as Dups
import qualified Data.Map as Map
import qualified Data.NonEmptyMap as NEM

import qualified Array as A
import qualified Graph

import qualified AST.Source as Src
import qualified AST.Prim.Fields as F
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.Shader as Shader
import qualified AST.Prim.Tag as Tag
import qualified AST.Prim.TypeIndex as T
import qualified AST.Prim.TypeMask as M
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Data.Index as Index
import qualified Elm.Float as EF
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Interface as I
import qualified Elm.String as ES
import qualified Nitpick.Utils as Nit
import qualified Reporting.Annotation as A



-- DECLARATIONS


newtype Decls =
  Decls (Graph.Sorted Decl)


data Decl
  = Decl Def
  | DeclRec Def [Def]


data Def
  = DValue_ (A.Located N.Name)                   Expr
  | DValue  (A.Located N.Name)                   Expr I.Type I.Annotation
  | DFunc_  (A.Located N.Name) Pattern [Pattern] Expr
  | DFunc   (A.Located N.Name) PArg    [PArg]    Expr I.Type I.Annotation



-- EXPRESSIONS
--
-- Many expressions carry an I.Annotation so that they have O(1) lookups during
-- type inference.


type Expr =
  A.Located Expr_


data Expr_
  = VarLocal N.Name
  | VarTopLevel ModuleName.Canonical N.Name
  | VarKernel Module.Kernel N.Name
  | VarForeign ModuleName.Canonical N.Name I.Annotation
  | VarDebug ModuleName.Canonical N.Name I.Annotation
  | VarOperator Op.Name ModuleName.Canonical N.Name I.Annotation -- CACHE real name for optimization
  --
  | CRecord  ModuleName.Canonical N.Name I.Annotation N.Name [N.Name]
  | CVariant ModuleName.Canonical N.Name I.Annotation Tag.Tag Int
  | CEnum    ModuleName.Canonical N.Name I.Annotation Tag.Tag
  | CVector  ModuleName.Canonical N.Name I.Annotation Int
  | CBox     ModuleName.Canonical N.Name I.Annotation
  | CVoid    ModuleName.Canonical N.Name I.Annotation
  --
  | Bool Bool
  | Chr Char
  | Str ES.String
  | Int Int
  | Float EF.Float
  | List [Expr]
  | Negate Expr
  | Binop Op.Name ModuleName.Canonical N.Name I.Annotation Expr Expr -- CACHE real name for optimization
  | Lambda Pattern [Pattern] Expr
  | Call Expr Expr [Expr]
  | If [(Expr, Expr)] Expr
  | LetValue Value Expr
  | LetFunc Func Expr
  | LetRec Func [Func] Expr
  | LetDestruct Pattern Expr Expr
  | Case Expr Branch [Branch]
  --
  | Unit
  | Pair Expr Expr
  | Triple Expr Expr Expr
  --
  | Empty
  | Record (F.Fields Expr)
  | Update N.Name Expr (F.Fields Update)
  | Access Expr (A.Located N.Name)
  | Accessor N.Name
  --
  | Shader Shader.Source Shader.Types


data Branch =
  Branch Pattern Expr


data Update =
  Update_ A.Region Expr



-- LET VALUES


data Value
  = Value_ (A.Located N.Name) Expr
  | Value  (A.Located N.Name) Expr I.Type I.Annotation -- TODO is I.Annotation needed?


data Func
  = Func_ (A.Located N.Name) Pattern [Pattern] Expr
  | Func  (A.Located N.Name) PArg    [PArg]    Expr I.Type I.Annotation -- TODO is I.Annotation needed?



-- PATTERNS


type Pattern =
  A.Located Pattern_


data Pattern_
  = PAnything
  | PVar N.Name
  | PAlias Pattern N.Name
  | PBool Bool N.Name Tag.Tag Nit.Variants
  | PChr Char
  | PStr ES.String
  | PInt Int
  | PUnit
  | PPair Pattern Pattern
  | PTriple Pattern Pattern Pattern
  | PRecord (NEM.Map N.Name ())
  | PList [Pattern]
  | PCons Pattern Pattern
  | PVariant N.Name PType Tag.Tag [PArg]
  | PEnum    N.Name PType Tag.Tag
  | PVector  N.Name PType PArg PArg [PArg]
  | PBox     N.Name PType I.Type Pattern
  | PVoid    N.Name PType


data PArg =
  PArg
    { _index :: Index.ZeroBased -- CACHE for destructors/errors
    , _type :: I.Type           -- CACHE for type inference
    , _arg :: Pattern
    }


data PType =
  PType
    { _p_home :: ModuleName.Canonical
    , _p_type :: T.Name
    , _p_mask :: M.Mask
    , _p_vars :: A.Array T.Var
    , _p_variants :: Nit.Variants
    }
    -- CACHE for type inference: _p_home, _p_type, _p_mask, and _p_vars
    -- CACHE for exhaustiveness checker: _p_variants



-- TYPE


data Annotation =
  Forall (A.Array T.Var) Type


type Type =
  A.Located Type_


data Type_
  = TLambda Type Type [Type]
  | TVar T.Index
  | TType  A.Region ModuleName.Canonical T.Name M.Mask (A.Array Type)
  | TAlias A.Region ModuleName.Canonical T.Name M.Mask (A.Array Type) I.Aliased
  | TEmpty
  | TUnit
  | TPair   Type Type
  | TTriple Type Type Type
  | TRecord (F.Fields Type)
  | TExtRec (F.Fields Type) (A.Located T.Index)


toIAnnotation :: Annotation -> I.Annotation
toIAnnotation (Forall xs t) =
  I.Forall xs (toIType t)


toIType :: Type -> I.Type
toIType =
    go
  where
    go (A.At _ tipe) =
      case tipe of
        TLambda x y ys      -> I.TLambda (go x) (go y) (map go ys)
        TVar i              -> I.TVar i
        TType  _ h n m xs   -> I.TType  h n m (A.map go xs)
        TAlias _ h n m xs t -> I.TAlias h n m (A.map go xs) t
        TUnit               -> I.TUnit
        TPair   a b         -> I.TPair   (go a) (go b)
        TTriple a b c       -> I.TTriple (go a) (go b) (go c)
        TEmpty              -> I.TEmpty
        TRecord fs          -> I.TRecord (F.map go fs)
        TExtRec fs ext      -> I.TExtRec (F.map go fs) (A.toValue ext)



-- MODULES


data Module =
  Module
    { _name    :: ModuleName.Canonical
    , _exports :: Exports
    , _docs    :: Src.Docs
    , _decls   :: Decls
    , _types   :: Map.Map T.Name TypeDecl
    , _binops  :: Map.Map Op.Name Binop
    , _style   :: Style
    }


data TypeDecl
  = Union I.Union
  | Alias I.Alias


data Binop = Binop_ N.Name Op.Associativity Op.Precedence
  deriving (Eq)



-- EXPORTS


data Exports
  = ExportEverything A.Region
  | Export
      { _e_types  :: Dups.Dict  T.Name A.Region Src.Privacy
      , _e_values :: Dups.Dict  N.Name A.Region ()
      , _e_binops :: Dups.Dict Op.Name A.Region ()
      }



-- EFFECTS


data Style
  = Normal
  | Ports (Map.Map N.Name Port)
  | Effect A.Region A.Region A.Region Manager


data Port
  = Incoming { _freeVars :: A.Array T.Var, _payload :: I.Type, _func :: I.Type }
  | Outgoing { _freeVars :: A.Array T.Var, _payload :: I.Type, _func :: I.Type }


data Manager
  = Cmd (EType CMD)
  | Sub (EType SUB)
  | Fx (EType CMD) (EType SUB)


data EType (k :: EFFECT) where
  EType :: T.Name -> M.Mask -> EType k


data EFFECT = CMD | SUB


