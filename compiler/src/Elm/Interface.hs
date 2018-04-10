{-# OPTIONS_GHC -Wall #-}
module Elm.Interface
  ( Interfaces
  , Interface(..)
  , Union
  , Alias
  , Binop(..)
  , fromModule
  , toPublicUnion
  , toPublicAlias
  , toUnionInternals
  , toAliasInternals
  )
  where


import Control.Monad (liftM4)
import Data.Binary
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Binop as Binop
import qualified Elm.Name as N
import qualified Reporting.Annotation as A



-- INTERFACES


type Interfaces =
  Map.Map ModuleName.Canonical Interface



-- INTERFACE


data Interface =
  Interface
    { _types   :: Map.Map N.Name Can.Annotation
    , _unions  :: Map.Map N.Name Union
    , _aliases :: Map.Map N.Name Alias
    , _binops  :: Map.Map N.Name Binop
    }


data Union
  = OpenUnion Can.Union
  | ClosedUnion Can.Union
  | PrivateUnion Can.Union


data Alias
  = PublicAlias Can.Alias
  | PrivateAlias Can.Alias


data Binop =
  Binop
    { _op_name :: N.Name
    , _op_annotation :: Can.Annotation
    , _op_associativity :: Binop.Associativity
    , _op_precedence :: Binop.Precedence
    }



-- FROM MODULE


fromModule :: Map.Map N.Name Can.Annotation -> Can.Module -> Interface
fromModule types (Can.Module _ _ exports _ unions aliases binops _) =
  Interface
    { _types = privatize exports types
    , _unions = privatizeUnions exports unions
    , _aliases = privatizeAliases exports aliases
    , _binops = privatize exports (Map.map (toOp types) binops)
    }


privatize :: Can.Exports -> Map.Map N.Name a -> Map.Map N.Name a
privatize exports dict =
  case exports of
    Can.ExportEverything _ ->
      dict

    Can.Export explicitExports ->
      Map.intersection dict explicitExports


toOp :: Map.Map N.Name Can.Annotation -> Can.Binop -> Binop
toOp types (Can.Binop_ associativity precedence name) =
  Binop name (types ! name) associativity precedence


privatizeUnions :: Can.Exports -> Map.Map N.Name Can.Union -> Map.Map N.Name Union
privatizeUnions exports unions =
  case exports of
    Can.ExportEverything _ ->
      Map.map OpenUnion unions

    Can.Export explicitExports ->
        Map.merge onLeft onRight onBoth explicitExports unions
      where
        onLeft = Map.dropMissing
        onRight = Map.mapMissing (\_ u -> PrivateUnion u)
        onBoth = Map.zipWithMatched $ \_ e u ->
          case A.toValue e of
            Can.ExportUnionOpen -> OpenUnion u
            Can.ExportUnionClosed -> ClosedUnion u
            _ -> error "impossible exports discovered in privatizeUnions"


privatizeAliases :: Can.Exports -> Map.Map N.Name Can.Alias -> Map.Map N.Name Alias
privatizeAliases exports aliases =
  case exports of
    Can.ExportEverything _ ->
      Map.map PublicAlias aliases

    Can.Export explicitExports ->
        Map.merge onLeft onRight onBoth explicitExports aliases
      where
        onLeft = Map.dropMissing
        onRight = Map.mapMissing (\_ a -> PrivateAlias a)
        onBoth = Map.zipWithMatched (\_ _ a -> PublicAlias a)



-- PUBLICIZE


toPublicUnion :: Union -> Maybe Can.Union
toPublicUnion iUnion =
  case iUnion of
    OpenUnion union ->
      Just union

    ClosedUnion (Can.Union vars _ _ opts) ->
      Just (Can.Union vars [] 0 opts)

    PrivateUnion _ ->
      Nothing


toPublicAlias :: Alias -> Maybe Can.Alias
toPublicAlias iAlias =
  case iAlias of
    PublicAlias alias ->
      Just alias

    PrivateAlias _ ->
      Nothing



-- INTERNALS


toUnionInternals :: Union -> Can.Union
toUnionInternals iUnion =
  case iUnion of
    OpenUnion union -> union
    ClosedUnion union -> union
    PrivateUnion union -> union


toAliasInternals :: Alias -> Can.Alias
toAliasInternals iAlias =
  case iAlias of
    PublicAlias alias -> alias
    PrivateAlias alias -> alias



-- BINARY


instance Binary Interface where
  get =
    liftM4 Interface get get get get

  put (Interface a b c d) =
    put a >> put b >> put c >> put d


instance Binary Union where
  put union =
    case union of
      OpenUnion    u -> putWord8 0 >> put u
      ClosedUnion  u -> putWord8 1 >> put u
      PrivateUnion u -> putWord8 2 >> put u

  get =
    do  n <- getWord8
        case n of
          0 -> OpenUnion <$> get
          1 -> ClosedUnion <$> get
          2 -> PrivateUnion <$> get
          _ -> error "binary encoding of Union was corrupted"


instance Binary Alias where
  put union =
    case union of
      PublicAlias  a -> putWord8 0 >> put a
      PrivateAlias a -> putWord8 1 >> put a

  get =
    do  n <- getWord8
        case n of
          0 -> PublicAlias <$> get
          1 -> PrivateAlias <$> get
          _ -> error "binary encoding of Alias was corrupted"


instance Binary Binop where
  get =
    liftM4 Binop get get get get

  put (Binop a b c d) =
    put a >> put b >> put c >> put d

