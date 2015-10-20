module AST.Type
    ( Raw, Raw'(..)
    , Canonical(..), Aliased(..)
    , Port(..), getPortType
    , deepDealias, iteratedDealias, dealias
    , collectLambdas
    , tuple
    ) where

import Control.Arrow (second)
import Data.Binary
import qualified Data.Map as Map

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


data Port t
    = Normal t
    | Signal { root :: t, arg :: t }
    deriving (Eq)


getPortType :: Port tipe -> tipe
getPortType portType =
  case portType of
    Normal tipe -> tipe
    Signal tipe _ -> tipe


tuple :: R.Region -> [Raw] -> Raw
tuple region types =
  let name = Var.Raw ("_Tuple" ++ show (length types))
  in
      A.A region (RApp (A.A region (RType name)) types)



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
    let go = dealiasHelp typeTable in
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
  gatherLambda getCanLambda tipe


gatherLambda :: (t -> Maybe (t,t)) -> t -> [t]
gatherLambda get tipe =
  case get tipe of
    Just (arg, body) ->
        arg : gatherLambda get body

    Nothing ->
        [tipe]


getRawLambda :: Raw -> Maybe (Raw, Raw)
getRawLambda (A.A _ tipe) =
  case tipe of
    RLambda arg body -> Just (arg, body)
    _ -> Nothing


getCanLambda :: Canonical -> Maybe (Canonical, Canonical)
getCanLambda tipe =
  case tipe of
    Lambda arg body -> Just (arg, body)
    _ -> Nothing



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

  get = do
      n <- getWord8
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

  get = do
      n <- getWord8
      case n of
        0 -> Holey <$> get
        1 -> Filled <$> get
        _ -> error "Error reading a valid type from serialized string"
