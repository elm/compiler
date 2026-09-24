{-# LANGUAGE ExtendedLiterals, MagicHash, TemplateHaskell #-}
module Elm.Interface
  ( Interface(..)
  , Union(..)
  , Alias(..)
  , Binop(..)
  , fromModule
  , toPublicUnion
  , toPublicAlias
  , DependencyInterface(..)
  , public
  , private
  , privatize
  , extractUnion
  , extractAlias
  --
  , eInterface, dInterface
  , eDependencyInterface, dDependencyInterface
  , eType, dType
  )
  where


import Control.Monad (liftM, liftM2, liftM3, liftM4, liftM5)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Utils as Map

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import qualified Crash

import qualified AST.Canonical as Can
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg



-- INTERFACE


data Interface =
  Interface
    { _home    :: Pkg.Name
    , _values  :: Map.Map N.Name Can.Annotation
    , _unions  :: Map.Map T.Name Union
    , _aliases :: Map.Map T.Name Alias
    , _binops  :: Map.Map Op.Name Binop
    }
  deriving (Eq)


data Union
  = OpenUnion Can.Union
  | ClosedUnion Can.Union
  | PrivateUnion Can.Union
  deriving (Eq)


data Alias
  = PublicAlias Can.Alias
  | PrivateAlias Can.Alias
  deriving (Eq)


data Binop =
  Binop
    { _op_name :: N.Name
    , _op_annotation :: Can.Annotation
    , _op_associativity :: Op.Associativity
    , _op_precedence :: Op.Precedence
    }
  deriving (Eq)



-- FROM MODULE


fromModule :: Pkg.Name -> Can.Module -> Map.Map N.Name Can.Annotation -> Interface
fromModule home (Can.Module _ exports _ _ unions aliases binops _) annotations =
  Interface
    { _home    = home
    , _values  = restrictValues  exports annotations
    , _unions  = restrictUnions  exports unions
    , _aliases = restrictAliases exports aliases
    , _binops  = restrictBinops  exports (Map.map (toOp annotations) binops)
    }


restrictValues :: Can.Exports -> Map.Map N.Name Can.Annotation -> Map.Map N.Name Can.Annotation
restrictValues exports dict =
  case exports of
    Can.ExportEverything _ ->
      dict

    Can.Export _ values _ ->
      Map.intersection dict values


restrictUnions :: Can.Exports -> Map.Map T.Name Can.Union -> Map.Map T.Name Union
restrictUnions exports unions =
  case exports of
    Can.ExportEverything _ ->
      Map.map OpenUnion unions

    Can.Export types _ _ ->
        Map.merge onLeft onRight onBoth types unions
      where
        onLeft = Map.dropMissing
        onRight = Map.mapMissing (\_ u -> PrivateUnion u)
        onBoth = Map.zipWithMatched $ \_ (_, export) union ->
          case export of
            Can.ExportUnionOpen   -> OpenUnion union
            Can.ExportUnionClosed -> ClosedUnion union
            Can.ExportAlias       -> $(Crash.crash 'restrictUnions) "impossible exports discovered"


restrictAliases :: Can.Exports -> Map.Map T.Name Can.Alias -> Map.Map T.Name Alias
restrictAliases exports aliases =
  case exports of
    Can.ExportEverything _ ->
      Map.map PublicAlias aliases

    Can.Export types _ _ ->
        Map.merge onLeft onRight onBoth types aliases
      where
        onLeft = Map.dropMissing
        onRight = Map.mapMissing (\_ a -> PrivateAlias a)
        onBoth = Map.zipWithMatched (\_ _ a -> PublicAlias a)


restrictBinops :: Can.Exports -> Map.Map Op.Name Binop -> Map.Map Op.Name Binop
restrictBinops exports dict =
  case exports of
    Can.ExportEverything _ ->
      dict

    Can.Export _ _ ops ->
      Map.intersection dict ops


toOp :: Map.Map N.Name Can.Annotation -> Can.Binop -> Binop
toOp types (Can.Binop_ associativity precedence name) =
  Binop name ($(Map.require 'toOp) name types N.toChars) associativity precedence



-- TO PUBLIC


toPublicUnion :: Union -> Maybe Can.Union
toPublicUnion iUnion =
  case iUnion of
    OpenUnion union                       -> Just union
    ClosedUnion (Can.Union vars _ _ opts) -> Just (Can.Union vars [] 0 opts)
    PrivateUnion _                        -> Nothing


toPublicAlias :: Alias -> Maybe Can.Alias
toPublicAlias iAlias =
  case iAlias of
    PublicAlias alias -> Just alias
    PrivateAlias _    -> Nothing



-- DEPENDENCY INTERFACE


data DependencyInterface
  = Public Interface
  | Private
      Pkg.Name
      (Map.Map T.Name Can.Union)
      (Map.Map T.Name Can.Alias)


public :: Interface -> DependencyInterface
public =
  Public


private :: Interface -> DependencyInterface
private (Interface pkg _ unions aliases _) =
  Private pkg (Map.map extractUnion unions) (Map.map extractAlias aliases)


extractUnion :: Union -> Can.Union
extractUnion iUnion =
  case iUnion of
    OpenUnion union -> union
    ClosedUnion union -> union
    PrivateUnion union -> union


extractAlias :: Alias -> Can.Alias
extractAlias iAlias =
  case iAlias of
    PublicAlias alias -> alias
    PrivateAlias alias -> alias


privatize :: DependencyInterface -> DependencyInterface
privatize di =
  case di of
    Public i -> private i
    Private _ _ _ -> di



-- BINARY


eInterface :: Interface -> E.Builder
eInterface (Interface h vs us as bs) =
  Pkg.eName h
  <> E.dict32 N.encode eAnnotation vs
  <> E.dict32 T.encode eUnion us
  <> E.dict32 T.encode eAlias as
  <> E.dict32 Op.encode eBinop bs


dInterface :: D.Decoder Interface
dInterface =
  liftM5 Interface
    Pkg.dName
    (D.dict32 N.decode dAnnotation)
    (D.dict32 T.decode dUnion)
    (D.dict32 T.decode dAlias)
    (D.dict32 Op.decode dBinop)


eUnion :: Union -> E.Builder
eUnion union =
  case union of
    OpenUnion    u -> E.u8# 0#Word8 <> eUnion_ u
    ClosedUnion  u -> E.u8# 1#Word8 <> eUnion_ u
    PrivateUnion u -> E.u8# 2#Word8 <> eUnion_ u


dUnion :: D.Decoder Union
dUnion =
  do  n <- D.u8
      case n of
        0 -> OpenUnion <$> dUnion_
        1 -> ClosedUnion <$> dUnion_
        2 -> PrivateUnion <$> dUnion_
        _ -> D.expecting "Union"


eAlias :: Alias -> E.Builder
eAlias alias =
  case alias of
    PublicAlias  a -> E.u8# 0#Word8 <> eAlias_ a
    PrivateAlias a -> E.u8# 1#Word8 <> eAlias_ a


dAlias :: D.Decoder Alias
dAlias =
  do  n <- D.u8
      case n of
        0 -> PublicAlias  <$> dAlias_
        1 -> PrivateAlias <$> dAlias_
        _ -> D.expecting "Alias"


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


eDependencyInterface :: DependencyInterface -> E.Builder
eDependencyInterface iface =
  case iface of
    Public  i     -> E.u8# 0#Word8 <> eInterface i
    Private n u a -> E.u8# 1#Word8 <> Pkg.eName n <> E.dict32 T.encode eUnion_ u <> E.dict32 T.encode eAlias_ a


dDependencyInterface :: D.Decoder DependencyInterface
dDependencyInterface =
  do  n <- D.u8
      case n of
        0 -> liftM  Public dInterface
        1 -> liftM3 Private Pkg.dName (D.dict32 T.decode dUnion_) (D.dict32 T.decode dAlias_)
        _ -> D.expecting "DependencyInterface"



-- BINARY HELPERS


eAlias_ :: Can.Alias -> E.Builder
eAlias_ (Can.Alias vs t) =
  E.list8 T.eVar vs <> eType t


dAlias_ :: D.Decoder Can.Alias
dAlias_ =
  liftM2 Can.Alias (D.list8 T.dVar) dType


eUnion_ :: Can.Union -> E.Builder
eUnion_ (Can.Union vs cs n opts) =
  E.list8 T.eVar vs <> E.list32 eCtor cs <> E.int n <> Can.eCtorOpts opts


dUnion_ :: D.Decoder Can.Union
dUnion_ =
  liftM4 Can.Union (D.list8 T.dVar) (D.list32 dCtor) D.int Can.dCtorOpts


eCtor :: Can.Ctor -> E.Builder
eCtor (Can.Ctor n i a t) =
  N.encode n <> Index.eZeroBased i <> E.int a <> E.list8 eType t


dCtor :: D.Decoder Can.Ctor
dCtor =
  liftM4 Can.Ctor N.decode Index.dZeroBased D.int (D.list8 dType)


eAnnotation :: Can.Annotation -> E.Builder
eAnnotation (Can.Forall vs t) =
  E.dict8 T.eVar (\() -> mempty) vs <> eType t


dAnnotation :: D.Decoder Can.Annotation
dAnnotation =
  liftM2 Can.Forall (D.dict8 T.dVar (pure ())) dType


eType :: Can.Type -> E.Builder
eType tipe =
  case tipe of
    Can.TLambda a b     -> E.u8# 0#Word8 <> eType a <> eType b
    Can.TVar x          -> E.u8# 1#Word8 <> T.eVar x
    Can.TRecord fs e    -> E.u8# 2#Word8 <> E.dict32 N.encode eFieldType fs <> E.maybe T.eVar e
    Can.TUnit           -> E.u8# 3#Word8
    Can.TTuple a b c    -> E.u8# 4#Word8 <> eType a <> eType b <> E.maybe eType c
    Can.TAlias h n xs a -> E.u8# 5#Word8 <> ModuleName.eCanonical h <> T.encode n <> E.list8 (\(x,t) -> T.eVar x <> eType t) xs <> eAliasType a
    Can.TType  h n xs   -> E.u8# 6#Word8 <> ModuleName.eCanonical h <> T.encode n <> E.list8 eType xs


dType :: D.Decoder Can.Type
dType =
  do  word <- D.u8
      case word of
        0 -> liftM2 Can.TLambda dType dType
        1 -> liftM  Can.TVar T.dVar
        2 -> liftM2 Can.TRecord (D.dict32 N.decode dFieldType) (D.maybe T.dVar)
        3 -> return Can.TUnit
        4 -> liftM3 Can.TTuple dType dType (D.maybe dType)
        5 -> liftM4 Can.TAlias ModuleName.dCanonical T.decode (D.list8 (liftM2 (,) T.dVar dType)) dAliasType
        6 -> liftM3 Can.TType ModuleName.dCanonical T.decode (D.list8 dType)
        _ -> D.expecting "Type"


eAliasType :: Can.AliasType -> E.Builder
eAliasType aliasType =
  case aliasType of
    Can.Holey  tipe -> E.u8 0 <> eType tipe
    Can.Filled tipe -> E.u8 1 <> eType tipe


dAliasType :: D.Decoder Can.AliasType
dAliasType =
  do  n <- D.u8
      case n of
        0 -> liftM Can.Holey dType
        1 -> liftM Can.Filled dType
        _ -> D.expecting "AliasType"


eFieldType :: Can.FieldType -> E.Builder
eFieldType (Can.FieldType i t) =
  E.u16 i <> eType t


dFieldType :: D.Decoder Can.FieldType
dFieldType =
  liftM2 Can.FieldType D.u16 dType

