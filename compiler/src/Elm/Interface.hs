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
  )
  where


import Control.Monad (liftM, liftM3, liftM4, liftM5)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Utils as Map
import qualified Data.Name as Name

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E

import qualified AST.Canonical as Can
import qualified AST.Utils.Binop as Binop
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A



-- INTERFACE


data Interface =
  Interface
    { _home    :: Pkg.Name
    , _values  :: Map.Map Name.Name Can.Annotation
    , _unions  :: Map.Map Name.Name Union
    , _aliases :: Map.Map Name.Name Alias
    , _binops  :: Map.Map Name.Name Binop
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
    { _op_name :: Name.Name
    , _op_annotation :: Can.Annotation
    , _op_associativity :: Binop.Associativity
    , _op_precedence :: Binop.Precedence
    }
  deriving (Eq)



-- FROM MODULE


fromModule :: Pkg.Name -> Can.Module -> Map.Map Name.Name Can.Annotation -> Interface
fromModule home (Can.Module _ exports _ _ unions aliases binops _) annotations =
  Interface
    { _home = home
    , _values = restrict exports annotations
    , _unions = restrictUnions exports unions
    , _aliases = restrictAliases exports aliases
    , _binops = restrict exports (Map.map (toOp annotations) binops)
    }


restrict :: Can.Exports -> Map.Map Name.Name a -> Map.Map Name.Name a
restrict exports dict =
  case exports of
    Can.ExportEverything _ ->
      dict

    Can.Export explicitExports ->
      Map.intersection dict explicitExports


toOp :: Map.Map Name.Name Can.Annotation -> Can.Binop -> Binop
toOp types (Can.Binop_ associativity precedence name) =
  Binop name ($(Map.require 'toOp) name types Name.toChars) associativity precedence


restrictUnions :: Can.Exports -> Map.Map Name.Name Can.Union -> Map.Map Name.Name Union
restrictUnions exports unions =
  case exports of
    Can.ExportEverything _ ->
      Map.map OpenUnion unions

    Can.Export explicitExports ->
        Map.merge onLeft onRight onBoth explicitExports unions
      where
        onLeft = Map.dropMissing
        onRight = Map.mapMissing (\_ union -> PrivateUnion union)
        onBoth = Map.zipWithMatched $ \_ (A.At _ export) union ->
          case export of
            Can.ExportUnionOpen -> OpenUnion union
            Can.ExportUnionClosed -> ClosedUnion union
            _ -> error "impossible exports discovered in restrictUnions"


restrictAliases :: Can.Exports -> Map.Map Name.Name Can.Alias -> Map.Map Name.Name Alias
restrictAliases exports aliases =
  case exports of
    Can.ExportEverything _ ->
      Map.map PublicAlias aliases

    Can.Export explicitExports ->
        Map.merge onLeft onRight onBoth explicitExports aliases
      where
        onLeft = Map.dropMissing
        onRight = Map.mapMissing (\_ a -> PrivateAlias a)
        onBoth = Map.zipWithMatched (\_ _ a -> PublicAlias a)



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
      (Map.Map Name.Name Can.Union)
      (Map.Map Name.Name Can.Alias)


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
  <> E.dict64 Name.encode Can.eAnnotation vs
  <> E.dict64 Name.encode eUnion us
  <> E.dict64 Name.encode eAlias as
  <> E.dict64 Name.encode eBinop bs


dInterface :: D.Decoder Interface
dInterface =
  liftM5 Interface
    Pkg.dName
    (D.dict64 Name.decode Can.dAnnotation)
    (D.dict64 Name.decode dUnion)
    (D.dict64 Name.decode dAlias)
    (D.dict64 Name.decode dBinop)


eUnion :: Union -> E.Builder
eUnion union =
  case union of
    OpenUnion    u -> E.u8# 0#Word8 <> Can.eUnion u
    ClosedUnion  u -> E.u8# 1#Word8 <> Can.eUnion u
    PrivateUnion u -> E.u8# 2#Word8 <> Can.eUnion u


dUnion :: D.Decoder Union
dUnion =
  do  n <- D.u8
      case n of
        0 -> OpenUnion <$> Can.dUnion
        1 -> ClosedUnion <$> Can.dUnion
        2 -> PrivateUnion <$> Can.dUnion
        _ -> D.expecting "Union"


eAlias :: Alias -> E.Builder
eAlias alias =
  case alias of
    PublicAlias  a -> E.u8# 0#Word8 <> Can.eAlias a
    PrivateAlias a -> E.u8# 1#Word8 <> Can.eAlias a


dAlias :: D.Decoder Alias
dAlias =
  do  n <- D.u8
      case n of
        0 -> PublicAlias  <$> Can.dAlias
        1 -> PrivateAlias <$> Can.dAlias
        _ -> D.expecting "Alias"


eBinop :: Binop -> E.Builder
eBinop (Binop n t a p) =
  Name.encode n <> Can.eAnnotation t <> Binop.eAssociativity a <> Binop.ePrecedence p


dBinop :: D.Decoder Binop
dBinop =
  liftM4 Binop Name.decode Can.dAnnotation Binop.dAssociativity Binop.dPrecedence


eDependencyInterface :: DependencyInterface -> E.Builder
eDependencyInterface iface =
  case iface of
    Public  i     -> E.u8# 0#Word8 <> eInterface i
    Private n u a -> E.u8# 1#Word8 <> Pkg.eName n <> E.dict64 Name.encode Can.eUnion u <> E.dict64 Name.encode Can.eAlias a


dDependencyInterface :: D.Decoder DependencyInterface
dDependencyInterface =
  do  n <- D.u8
      case n of
        0 -> liftM  Public dInterface
        1 -> liftM3 Private Pkg.dName (D.dict64 Name.decode Can.dUnion) (D.dict64 Name.decode Can.dAlias)
        _ -> D.expecting "DependencyInterface"

