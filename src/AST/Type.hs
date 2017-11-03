{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Type
    ( Raw, Raw'(..)
    , Canonical(..), Aliased(..)
    , Super(..)
    , deepDealias, iteratedDealias, dealias
    , collectLambdas
    , cmd, sub
    )
    where


import Control.Monad (liftM, liftM2, liftM3, liftM4, replicateM)
import Data.Binary
import qualified Data.Map as Map

import qualified AST.Module.Name as ModuleName
import Elm.Name (Name)
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- DEFINITION


type Raw =
    A.Located Raw'


data Raw'
  = RLambda Raw Raw
  | RVar Name
  | RType R.Region (Maybe Name) Name [Raw]
  | RRecord [(A.Located Name, Raw)] (Maybe Raw)
  | RUnit
  | RTuple Raw Raw [Raw]


data Canonical
  = Lambda Canonical Canonical
  | Var Name
  | Type ModuleName.Canonical Name [Canonical]
  | Record (Map.Map Name Canonical) (Maybe Canonical)
  | Unit
  | Tuple Canonical Canonical (Maybe Canonical)
  | Aliased ModuleName.Canonical Name [(Name, Canonical)] (Aliased Canonical)
  deriving (Eq, Ord)


data Aliased t
  = Holey t
  | Filled t
  deriving (Eq, Ord)


data Super
  = Number
  | Comparable
  | Appendable
  | CompAppend
  deriving (Eq)



-- CONSTRUCT USEFUL TYPES


cmd :: ModuleName.Canonical -> Name -> Canonical
cmd home tipe =
  Lambda
    (Type home tipe [Var "msg"])
    (Type ModuleName.cmd "Cmd" [Var "msg"])


sub :: ModuleName.Canonical -> Name -> Canonical
sub home tipe =
  Lambda
    (Type home tipe [Var "msg"])
    (Type ModuleName.sub "Sub" [Var "msg"])



-- DEALIASING


deepDealias :: Canonical -> Canonical
deepDealias tipe =
  case tipe of
    Lambda a b ->
      Lambda (deepDealias a) (deepDealias b)

    Var _ ->
      tipe

    Record fields ext ->
      Record (Map.map deepDealias fields) (fmap deepDealias ext)

    Aliased _ _ args tipe' ->
      deepDealias (dealias args tipe')

    Type home name args ->
      Type home name (map deepDealias args)

    Unit ->
      Unit

    Tuple a b c ->
      Tuple (deepDealias a) (deepDealias b) (fmap deepDealias c)


iteratedDealias :: Canonical -> Canonical
iteratedDealias tipe =
  case tipe of
    Aliased _ _ args realType ->
      iteratedDealias (dealias args realType)

    _ ->
      tipe


dealias :: [(Name, Canonical)] -> Aliased Canonical -> Canonical
dealias args aliasType =
  case aliasType of
    Holey tipe ->
      dealiasHelp (Map.fromList args) tipe

    Filled tipe ->
      tipe


dealiasHelp :: Map.Map Name Canonical -> Canonical -> Canonical
dealiasHelp typeTable tipe =
  let
    go =
      dealiasHelp typeTable
  in
  case tipe of
    Lambda a b ->
      Lambda (go a) (go b)

    Var x ->
      Map.findWithDefault tipe x typeTable

    Record fields ext ->
      Record (Map.map go fields) (fmap go ext)

    Aliased home name args t' ->
      Aliased home name (map (fmap go) args) t'

    Type home name args ->
      Type home name (map go args)

    Unit ->
      Unit

    Tuple a b c ->
      Tuple (go a) (go b) (fmap go c)



-- COLLECT LAMBDAS


collectLambdas :: Canonical -> [Canonical]
collectLambdas tipe =
  case tipe of
    Lambda arg result ->
      arg : collectLambdas result

    _ ->
      [tipe]



-- BINARY


instance Binary Canonical where
  put tipe =
    case tipe of
      Lambda a b        -> putWord8 0 >> put a >> put b
      Var a             -> putWord8 1 >> put a
      Record a b        -> putWord8 2 >> put a >> put b
      Unit              -> putWord8 3
      Tuple a b c       -> putWord8 4 >> put a >> put b >> put c
      Aliased a b c d   -> putWord8 5 >> put a >> put b >> put c >> put d
      Type home name ts ->
        let
          potentialWord =
            length ts + 7
        in
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
          0 -> liftM2 Lambda get get
          1 -> liftM  Var get
          2 -> liftM2 Record get get
          3 -> return Unit
          4 -> liftM3 Tuple get get get
          5 -> liftM4 Aliased get get get get
          6 -> liftM3 Type get get get
          n -> liftM3 Type get get (replicateM (fromIntegral (n - 7)) get)


instance Binary t => Binary (Aliased t) where
  put aliasType =
    case aliasType of
      Holey tipe ->
        putWord8 0 >> put tipe

      Filled tipe ->
        putWord8 1 >> put tipe

  get =
    do  n <- getWord8
        case n of
          0 -> liftM Holey get
          1 -> liftM Filled get
          _ -> error "Error reading a valid type from serialized string"
