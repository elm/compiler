module AST.Utils.Type
  ( delambda
  , dealias
  , deepDealias
  , iteratedDealias
  )
  where


import qualified Data.Map as Map

import AST.Canonical (Type(..), AliasType(..), FieldType(..))
import qualified AST.Prim.TypeVar as T



-- DELAMBDA


delambda :: Type -> [Type]
delambda tipe =
  case tipe of
    TLambda arg result ->
      arg : delambda result

    _ ->
      [tipe]



-- DEALIAS
--
-- TODO BUG record extensions seem to be skipped during dealiasing


dealias :: [(T.Var, Type)] -> AliasType -> Type
dealias args aliasType =
  case aliasType of
    Holey  tipe -> dealiasHelp (Map.fromList args) tipe
    Filled tipe -> tipe


dealiasHelp :: Map.Map T.Var Type -> Type -> Type
dealiasHelp typeTable =
    go
  where
    go tipe =
      case tipe of
        TLambda a b     -> TLambda (go a) (go b)
        TVar x          -> Map.findWithDefault tipe x typeTable
        TRecord fs e    -> TRecord (Map.map (dealiasField typeTable) fs) e
        TAlias h n xs t -> TAlias h n (map (fmap go) xs) t
        TType  h n xs   -> TType  h n (map go xs)
        TUnit           -> TUnit
        TTuple  a b mc  -> TTuple (go a) (go b) (fmap go mc)


dealiasField :: Map.Map T.Var Type -> FieldType -> FieldType
dealiasField typeTable (FieldType index tipe) =
  FieldType index (dealiasHelp typeTable tipe)



-- DEEP DEALIAS


deepDealias :: Type -> Type
deepDealias tipe =
  case tipe of
    TLambda a b     -> TLambda (deepDealias a) (deepDealias b)
    TVar _          -> tipe
    TRecord fs x    -> TRecord (Map.map deepDealiasField fs) x
    TAlias _ _ xs t -> deepDealias (dealias xs t)
    TType h n xs    -> TType h n (map deepDealias xs)
    TUnit           -> TUnit
    TTuple a b mc   -> TTuple (deepDealias a) (deepDealias b) (fmap deepDealias mc)


deepDealiasField :: FieldType -> FieldType
deepDealiasField (FieldType index tipe) =
  FieldType index (deepDealias tipe)



-- ITERATED DEALIAS


iteratedDealias :: Type -> Type
iteratedDealias tipe =
  case tipe of
    TAlias _ _ xs t -> iteratedDealias (dealias xs t)
    _               -> tipe
