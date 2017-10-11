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

import Control.Arrow (second)
import Control.Monad (liftM, liftM2, liftM3, replicateM)
import Data.Binary
import qualified Data.Map as Map
import Data.Text (Text)

import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- DEFINITION


type Raw =
    A.Located Raw'


data Raw'
    = RLambda Raw Raw
    | RVar Text
    | RType R.Region (Maybe Text) Text [Raw]
    | RRecord [(A.Located Text, Raw)] (Maybe Raw)
    | RUnit
    | RTuple Raw Raw [Raw]


data Canonical
    = Lambda Canonical Canonical
    | Var Text
    | Type Var.Canonical [Canonical]
    | Record [(Text, Canonical)] (Maybe Canonical)
    | Unit
    | Tuple Canonical Canonical [Canonical]
    | Aliased Var.Canonical [(Text, Canonical)] (Aliased Canonical)
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


cmd :: ModuleName.Canonical -> Text -> Canonical
cmd =
  effect Var.cmd


sub :: ModuleName.Canonical -> Text -> Canonical
sub =
  effect Var.sub


effect :: Var.Canonical -> ModuleName.Canonical -> Text -> Canonical
effect effectName moduleName tipe =
  Lambda
    (Type (Var.fromModule moduleName tipe) [Var "msg"])
    (Type effectName [Var "msg"])



-- DEALIASING


deepDealias :: Canonical -> Canonical
deepDealias tipe =
  case tipe of
    Lambda a b ->
      Lambda (deepDealias a) (deepDealias b)

    Var _ ->
      tipe

    Record fields ext ->
      Record (map (second deepDealias) fields) (fmap deepDealias ext)

    Aliased _name args tipe' ->
      deepDealias (dealias args tipe')

    Type name args ->
      Type name (map deepDealias args)

    Unit ->
      Unit

    Tuple a b cs ->
      Tuple (deepDealias a) (deepDealias b) (map deepDealias cs)


iteratedDealias :: Canonical -> Canonical
iteratedDealias tipe =
  case tipe of
    Aliased _ args realType ->
      iteratedDealias (dealias args realType)

    _ ->
      tipe


dealias :: [(Text, Canonical)] -> Aliased Canonical -> Canonical
dealias args aliasType =
  case aliasType of
    Holey tipe ->
      dealiasHelp (Map.fromList args) tipe

    Filled tipe ->
      tipe


dealiasHelp :: Map.Map Text Canonical -> Canonical -> Canonical
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
      Record (map (second go) fields) (fmap go ext)

    Aliased original args t' ->
      Aliased original (map (second go) args) t'

    Type name args ->
      Type name (map go args)

    Unit ->
      Unit

    Tuple a b cs ->
      Tuple (go a) (go b) (map go cs)



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
      Lambda t1 t2       -> putWord8 0 >> put t1 >> put t2
      Var x              -> putWord8 1 >> put x
      Record fs ext      -> putWord8 2 >> put fs >> put ext
      Unit               -> putWord8 3
      Tuple a b cs       -> putWord8 4 >> put a >> put b >> put cs
      Aliased var args t -> putWord8 5 >> put var >> put args >> put t
      Type name ts       ->
        let
          potentialWord =
            length ts + 7
        in
          if potentialWord <= fromIntegral (maxBound :: Word8) then
            do  putWord8 (fromIntegral potentialWord)
                put name
                mapM_ put ts
          else
            putWord8 6 >> put name >> put ts

  get =
    do  word <- getWord8
        case word of
          0 -> liftM2 Lambda get get
          1 -> liftM  Var get
          2 -> liftM2 Record get get
          3 -> return Unit
          4 -> liftM3 Tuple get get get
          5 -> liftM3 Aliased get get get
          6 -> liftM2 Type get get
          n -> liftM2 Type get (replicateM (fromIntegral (n - 7)) get)


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
