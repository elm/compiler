{-# OPTIONS_GHC -Wall #-}
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
  )
  where


import Control.Monad (liftM, liftM3, liftM4, liftM5)
import Data.Binary
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Name as Name

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
  Binop name (types ! name) associativity precedence


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


instance Binary Interface where
  get = liftM5 Interface get get get get get
  put (Interface a b c d e) = put a >> put b >> put c >> put d >> put e


instance Binary Union where
  put union =
    case union of
      OpenUnion    u -> putWord8 0 >> put u
      ClosedUnion  u -> putWord8 1 >> put u
      PrivateUnion u -> putWord8 2 >> put u

  get =
    do  n <- getWord8
        case n of
          0 -> liftM OpenUnion get
          1 -> liftM ClosedUnion get
          2 -> liftM PrivateUnion get
          _ -> fail "binary encoding of Union was corrupted"


instance Binary Alias where
  put union =
    case union of
      PublicAlias  a -> putWord8 0 >> put a
      PrivateAlias a -> putWord8 1 >> put a

  get =
    do  n <- getWord8
        case n of
          0 -> liftM PublicAlias get
          1 -> liftM PrivateAlias get
          _ -> fail "binary encoding of Alias was corrupted"


instance Binary Binop where
  get =
    liftM4 Binop get get get get

  put (Binop a b c d) =
    put a >> put b >> put c >> put d


instance Binary DependencyInterface where
  put union =
    case union of
      Public  a     -> putWord8 0 >> put a
      Private a b c -> putWord8 1 >> put a >> put b >> put c

  get =
    do  n <- getWord8
        case n of
          0 -> liftM  Public get
          1 -> liftM3 Private get get get
          _ -> fail "binary encoding of DependencyInterface was corrupted"
