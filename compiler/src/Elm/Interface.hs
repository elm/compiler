{-# LANGUAGE ExtendedLiterals, MagicHash, TemplateHaskell #-}
module Elm.Interface
  ( Interface(..)
  , TypeDecl(..)
  , Binop(..)
  --
  , Annotation(..)
  , Type(..)
  --
  , Alias(..)
  , Aliased(..)
  --
  , Union(..)
  , Variants(..)
  , Variant(..)
  --
  , dealias
  , deepDealias
  --
  , DependencyInterface(..)
  , PrivateTypeDecl(..)
  , public
  , private
  , privatize
  , extract
  --
  , eInterface, dInterface
  , eDependencyInterface, dDependencyInterface
  , eType, dType
  )
  where


import Control.Monad (liftM, liftM2, liftM3, liftM4, liftM5)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map

import qualified Array as A
import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Crash

import qualified AST.Prim.Fields as F
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.Tag as Tag
import qualified AST.Prim.TypeIndex as T
import qualified AST.Prim.TypeMask as M
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg



-- INTERFACE


data Interface =
  Interface
    { _home   :: Pkg.Name
    , _values :: Map.Map N.Name Annotation
    , _types  :: Map.Map T.Name TypeDecl
    , _binops  :: Map.Map Op.Name Binop
    }
  deriving (Eq)


data TypeDecl
  = UnionOpen    Union
  | UnionClosed  Union
  | UnionPrivate Union
  --
  | AliasPublic  Alias
  | AliasPrivate Alias
  deriving (Eq)


data Binop =
  Binop N.Name Annotation Op.Associativity Op.Precedence
  deriving (Eq)



-- TYPES
--
-- The fields of TRecord carry an index so that they can be resorted into the
-- order they appeared in the source file. THIS ORDER IS NOT GUARANTEED TO BE
-- CORRECT WHEN THE TYPE IS INFERRED, but it is correct in all other cases.
--
-- For example, adding type annotations will guarantee that your Annotation
-- uses the explicitly stated field order. And most importantly, Aliased types
-- preserve the source order to make sure the record constructor arguments are
-- in the right order.


data Annotation =
  Forall (A.Array T.Var) Type
  deriving (Eq)


data Type
  = TLambda Type Type [Type]
  | TVar T.Index
  | TType  ModuleName.Canonical T.Name M.Mask (A.Array Type)
  | TAlias ModuleName.Canonical T.Name M.Mask (A.Array Type) Aliased
  | TUnit
  | TPair Type Type
  | TTriple Type Type Type
  | TEmpty
  | TRecord (F.Fields Type)
  | TExtRec (F.Fields Type) T.Index
  deriving (Eq)



-- ALIAS


data Alias = Alias M.Mask (A.Array T.Var) Aliased
  deriving (Eq)


newtype Aliased = Aliased Type
  deriving (Eq)



-- UNION


data Union =
  Union M.Mask (A.Array T.Var) Variants
  deriving (Eq)


data Variants
  = Variants (Tag.Table Variant)
  | Enum     (Tag.Table N.Name)
  | Vector N.Name Type Type [Type]
  | Box    N.Name Type
  | Void   N.Name
  deriving (Eq)


data Variant = Variant N.Name [Type]
  deriving (Eq)



-- DEALIAS


dealias :: A.Array Type -> Aliased -> Type
dealias args (Aliased aliased) =
    go aliased
  where
    go tipe =
      case tipe of
        TLambda x y zs    -> TLambda (go x) (go y) (map go zs)
        TVar   i          -> T.get i args
        TType  h n m xs   -> TType  h n m (A.map go xs)
        TAlias h n m xs t -> TAlias h n m (A.map go xs) t
        TUnit             -> TUnit
        TPair   a b       -> TPair (go a) (go b)
        TTriple a b c     -> TTriple (go a) (go b) (go c)
        TEmpty            -> TEmpty
        TRecord fs        -> TRecord (F.map go fs)
        TExtRec fs ext    -> extend (F.map go fs) (T.get ext args)

    extend fields tipe =
      case tipe of
        TEmpty            -> TRecord fields
        TRecord fs        -> TRecord (F.extend fields fs)
        TExtRec fs ext    -> extend  (F.extend fields fs) (T.get ext args)
        TVar i            -> TExtRec fields i
        TAlias _ _ _ xs t -> extend fields (dealias (A.map go xs) t)
        TType  _ _ _ _    -> $(Crash.crash 'dealias) "ran into invalid record extension (Union)"
        TLambda _ _ _     -> $(Crash.crash 'dealias) "ran into invalid record extension (Lambda)"
        TUnit             -> $(Crash.crash 'dealias) "ran into invalid record extension (Unit)"
        TPair _ _         -> $(Crash.crash 'dealias) "ran into invalid record extension (Pair)"
        TTriple _ _ _     -> $(Crash.crash 'dealias) "ran into invalid record extension (Triple)"



-- DEEP DEALIAS


deepDealias :: Type -> Type
deepDealias tipe =
  case tipe of
    TLambda a b cs    -> TLambda (deepDealias a) (deepDealias b) (map deepDealias cs)
    TVar _            -> tipe
    TType  h n m xs   -> TType h n m (A.map deepDealias xs)
    TAlias _ _ _ xs t -> deepDealias (dealias xs t)
    TUnit             -> TUnit
    TPair   a b       -> TPair   (deepDealias a) (deepDealias b)
    TTriple a b c     -> TTriple (deepDealias a) (deepDealias b) (deepDealias c)
    TEmpty            -> TEmpty
    TRecord fs        -> TRecord (F.map deepDealias fs)
    TExtRec fs x      -> TExtRec (F.map deepDealias fs) x



-- DEPENDENCY INTERFACE


data DependencyInterface
  = Public Interface
  | Private Pkg.Name (Map.Map T.Name PrivateTypeDecl)


data PrivateTypeDecl
  = PrivateUnion Union
  | PrivateAlias Alias


public :: Interface -> DependencyInterface
public =
  Public


private :: Interface -> DependencyInterface
private (Interface pkg _ types _) =
  Private pkg (Map.map (extract PrivateUnion PrivateAlias) types)


extract :: (Union -> a) -> (Alias -> a) -> TypeDecl -> a
extract okU okA decl =
  case decl of
    UnionOpen    u -> okU u
    UnionClosed  u -> okU u
    UnionPrivate u -> okU u
    AliasPublic  a -> okA a
    AliasPrivate a -> okA a


privatize :: DependencyInterface -> DependencyInterface
privatize di =
  case di of
    Public  i   -> private i
    Private _ _ -> di



--------------------------------------------------------------------------------
-- BINARY ----------------------------------------------------------------------
--------------------------------------------------------------------------------



-- INTERFACE


eInterface :: Interface -> E.Builder
eInterface (Interface h vs ts bs) =
  Pkg.eName h
  <> E.dict32 N.encode eAnnotation vs
  <> E.dict32 T.encode eTypeDecl ts
  <> E.dict32 Op.encode eBinop bs


dInterface :: D.Decoder Interface
dInterface =
  liftM4 Interface
    Pkg.dName
    (D.dict32 N.decode dAnnotation)
    (D.dict32 T.decode dTypeDecl)
    (D.dict32 Op.decode dBinop)



-- TYPE DECL


eTypeDecl :: TypeDecl -> E.Builder
eTypeDecl union =
  case union of
    UnionOpen    u -> E.u8# 0#Word8 <> eUnion u
    UnionClosed  u -> E.u8# 1#Word8 <> eUnion u
    UnionPrivate u -> E.u8# 2#Word8 <> eUnion u
    AliasPublic  a -> E.u8# 3#Word8 <> eAlias a
    AliasPrivate a -> E.u8# 4#Word8 <> eAlias a


dTypeDecl :: D.Decoder TypeDecl
dTypeDecl =
  do  n <- D.u8
      case n of
        0 -> UnionOpen    <$> dUnion
        1 -> UnionClosed  <$> dUnion
        2 -> UnionPrivate <$> dUnion
        3 -> AliasPublic  <$> dAlias
        4 -> AliasPrivate <$> dAlias
        _ -> D.expecting "TypeDecl"



-- BINOP


eBinop :: Binop -> E.Builder
eBinop (Binop n t a p) =
  N.encode n <> eAnnotation t <> eAssociativity a <> ePrecedence p


dBinop :: D.Decoder Binop
dBinop =
  liftM4 Binop N.decode dAnnotation dAssociativity dPrecedence


dPrecedence :: D.Decoder Op.Precedence
dPrecedence =
  coerce D.u8


ePrecedence :: Op.Precedence -> E.Builder
ePrecedence (Op.Precedence n) =
  E.u8 n


dAssociativity :: D.Decoder Op.Associativity
dAssociativity =
  do  n <- D.u8
      case n of
        0 -> return Op.Left
        1 -> return Op.Non
        2 -> return Op.Right
        _ -> D.expecting "Associativity"


eAssociativity :: Op.Associativity -> E.Builder
eAssociativity assoc =
  case assoc of
    Op.Left  -> E.u8# 0#Word8
    Op.Non   -> E.u8# 1#Word8
    Op.Right -> E.u8# 2#Word8



-- DEPENDENCY INTERFACE


eDependencyInterface :: DependencyInterface -> E.Builder
eDependencyInterface iface =
  case iface of
    Public  i   -> E.u8# 0#Word8 <> eInterface i
    Private n t -> E.u8# 1#Word8 <> Pkg.eName n <> E.dict32 T.encode ePrivateTypeDecl t


dDependencyInterface :: D.Decoder DependencyInterface
dDependencyInterface =
  do  n <- D.u8
      case n of
        0 -> liftM  Public dInterface
        1 -> liftM2 Private Pkg.dName (D.dict32 T.decode dPrivateTypeDecl)
        _ -> D.expecting "DependencyInterface"


ePrivateTypeDecl :: PrivateTypeDecl -> E.Builder
ePrivateTypeDecl union =
  case union of
    PrivateUnion u -> E.u8# 0#Word8 <> eUnion u
    PrivateAlias a -> E.u8# 1#Word8 <> eAlias a


dPrivateTypeDecl :: D.Decoder PrivateTypeDecl
dPrivateTypeDecl =
  do  n <- D.u8
      case n of
        0 -> PrivateUnion <$> dUnion
        1 -> PrivateAlias <$> dAlias
        _ -> D.expecting "PrivateTypeDecl"



-- ALIAS


eAlias :: Alias -> E.Builder
eAlias (Alias m vs t) =
  M.eMask m <> E.array8 T.eVar vs <> eAliased t


dAlias :: D.Decoder Alias
dAlias =
  liftM3 Alias M.dMask (D.array8 T.dVar) dAliased



-- UNION


eUnion :: Union -> E.Builder
eUnion (Union m xs vs) =
  M.eMask m <> E.array8 T.eVar xs <> eVariants vs


dUnion :: D.Decoder Union
dUnion =
  liftM3 Union M.dMask (D.array8 T.dVar) dVariants



-- VARIANTS


eVariants :: Variants -> E.Builder
eVariants variants =
  case variants of
    Variants     vs -> E.u8 0 <> Tag.eTable eVariant vs
    Enum         es -> E.u8 1 <> Tag.eTable N.encode es
    Vector n a b cs -> E.u8 2 <> N.encode n <> eType a <> eType b <> E.list8 eType cs
    Box    n a      -> E.u8 3 <> N.encode n <> eType a
    Void   n        -> E.u8 4 <> N.encode n


dVariants :: D.Decoder Variants
dVariants =
  do  tag <- D.u8
      case tag of
        0 -> D.map1 Variants (Tag.dTable dVariant)
        1 -> D.map1 Enum     (Tag.dTable N.decode)
        2 -> D.map4 Vector N.decode dType dType (D.list8 dType)
        3 -> D.map2 Box    N.decode dType
        4 -> D.map1 Void   N.decode
        _ -> D.expecting "Variants"



-- VARIANT


eVariant :: Variant -> E.Builder
eVariant (Variant n ts) =
  N.encode n <> E.list8 eType ts


dVariant :: D.Decoder Variant
dVariant =
  liftM2 Variant N.decode (D.list8 dType)



-- ANNOTATION


eAnnotation :: Annotation -> E.Builder
eAnnotation (Forall xs t) =
  E.array8 T.eVar xs <> eType t


dAnnotation :: D.Decoder Annotation
dAnnotation =
  liftM2 Forall (D.array8 T.dVar) dType



-- TYPE


eType :: Type -> E.Builder
eType tipe =
  case tipe of
    TLambda a b cs    -> E.u8# 0#Word8 <> eType a <> eType b <> E.list8 eType cs
    TVar i            -> E.u8# 1#Word8 <> T.eIndex i
    TType  h n m xs   -> E.u8# 2#Word8 <> ModuleName.eCanonical h <> T.encode n <> M.eMask m <> E.array8 eType xs
    TAlias h n m xs a -> E.u8# 3#Word8 <> ModuleName.eCanonical h <> T.encode n <> M.eMask m <> E.array8 eType xs <> eAliased a
    TUnit             -> E.u8# 4#Word8
    TPair   a b       -> E.u8# 5#Word8 <> eType a <> eType b
    TTriple a b c     -> E.u8# 6#Word8 <> eType a <> eType b <> eType c
    TEmpty            -> E.u8# 7#Word8
    TRecord fs        -> E.u8# 8#Word8 <> F.encode eType fs
    TExtRec fs i      -> E.u8# 9#Word8 <> F.encode eType fs <> T.eIndex i


dType :: D.Decoder Type
dType =
  do  word <- D.u8
      case word of
        0 -> liftM3 TLambda dType dType (D.list8 dType)
        1 -> liftM  TVar T.dIndex
        2 -> liftM4 TType  ModuleName.dCanonical T.decode M.dMask (D.array8 dType)
        3 -> liftM5 TAlias ModuleName.dCanonical T.decode M.dMask (D.array8 dType) dAliased
        4 -> pure   TUnit
        5 -> liftM2 TPair   dType dType
        6 -> liftM3 TTriple dType dType dType
        7 -> pure   TEmpty
        8 -> liftM  TRecord (F.decode dType)
        9 -> liftM2 TExtRec (F.decode dType) T.dIndex
        _ -> D.expecting "Type"



-- ALIASED


eAliased :: Aliased -> E.Builder
eAliased =
  coerce eType


dAliased :: D.Decoder Aliased
dAliased =
  coerce dType

