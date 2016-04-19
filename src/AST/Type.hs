module AST.Type
    ( Raw, Raw'(..)
    , Canonical(..), Aliased(..)
    , deepDealias, iteratedDealias, dealias
    , collectLambdas
    , tuple, cmd, sub
    )
    where

import Control.Arrow (second)
import Data.Binary
import qualified Data.Map as Map

import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- DEFINITION


type Raw =
    A.Located Raw'


data Raw'
    = RLambda Raw Raw
    | RVar String
    | RType Var.Raw
    | RApp Raw [Raw]
    | RRecord [(String, Raw)] (Maybe Raw)


data Canonical
    = Lambda Canonical Canonical
    | Var String
    | Type Var.Canonical
    | App Canonical [Canonical]
    | Record [(String, Canonical)] (Maybe Canonical)
    | Aliased Var.Canonical [(String, Canonical)] (Aliased Canonical)
    deriving (Eq, Ord)


data Aliased t
    = Holey t
    | Filled t
    deriving (Eq, Ord)



-- CONSTRUCT USEFUL TYPES


tuple :: R.Region -> [Raw] -> Raw
tuple region types =
  let
    name =
      Var.Raw ("_Tuple" ++ show (length types))
  in
    A.A region (RApp (A.A region (RType name)) types)


cmd :: ModuleName.Canonical -> String -> Canonical
cmd =
  effect Var.cmd


sub :: ModuleName.Canonical -> String -> Canonical
sub =
  effect Var.sub


effect :: Var.Canonical -> ModuleName.Canonical -> String -> Canonical
effect effectName moduleName tipe =
  Lambda
    (App (Type (Var.fromModule moduleName tipe)) [Var "msg"])
    (App (Type effectName) [Var "msg"])



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

    Type _ ->
      tipe

    App f args ->
      App (deepDealias f) (map deepDealias args)


iteratedDealias :: Canonical -> Canonical
iteratedDealias tipe =
  case tipe of
    Aliased _ args realType ->
      iteratedDealias (dealias args realType)

    _ ->
      tipe


dealias :: [(String, Canonical)] -> Aliased Canonical -> Canonical
dealias args aliasType =
  case aliasType of
    Holey tipe ->
      dealiasHelp (Map.fromList args) tipe

    Filled tipe ->
      tipe


dealiasHelp :: Map.Map String Canonical -> Canonical -> Canonical
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

    Type _ ->
      tipe

    App f args ->
      App (go f) (map go args)



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
      Lambda t1 t2 ->
        putWord8 0 >> put t1 >> put t2

      Var x ->
        putWord8 1 >> put x

      Type name ->
        putWord8 2 >> put name

      App t1 t2 ->
        putWord8 3 >> put t1 >> put t2

      Record fs ext ->
        putWord8 4 >> put fs >> put ext

      Aliased var args t ->
        putWord8 5 >> put var >> put args >> put t

  get =
    do  n <- getWord8
        case n of
          0 -> Lambda <$> get <*> get
          1 -> Var <$> get
          2 -> Type <$> get
          3 -> App <$> get <*> get
          4 -> Record <$> get <*> get
          5 -> Aliased <$> get <*> get <*> get
          _ -> error "Error reading a valid type from serialized string"


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
          0 -> Holey <$> get
          1 -> Filled <$> get
          _ -> error "Error reading a valid type from serialized string"
