{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Environment
    ( Env
    , Dict
    , initialize
    , get, freshDataScheme, ctorNames
    , addValues
    , instantiateFlex
    , instantiateRigid
    )
    where

import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)

import qualified AST.Helpers as Help
import qualified AST.Type as T
import qualified AST.Variable as Var
import qualified AST.Module as Module
import Type.Type



-- ENVIRONMENT


data Env =
  Env
    { _ctor  :: Dict Instantiator
    , _value :: Dict Type
    }


type Instantiator =
  IO (Int, [Variable], [Type], Type)


type Dict a =
  Map.Map Text a


initialize :: [Module.CanonicalUnion] -> Env
initialize unions =
  Env (makeCtors unions) Map.empty



-- CONSTRUCTORS


makeCtors :: [Module.CanonicalUnion] -> Dict Instantiator
makeCtors unions =
  let
    list t =
      AppN Var.list [t]

    inst :: Int -> ([Type] -> ([Type], Type)) -> Instantiator
    inst numTVars tipe =
      do  vars <- mapM (\_ -> mkFlexVar) [1..numTVars]
          let (args, result) = tipe (map (VarN) vars)
          return (length args, vars, args, result)

    tupleCtor size =
      let
        name =
          Help.makeTuple size
      in
        (name, inst size $ \vs -> (vs, AppN (Var.tuple size) vs))

    builtins :: Dict Instantiator
    builtins =
      Map.fromList $
        [ ("[]", inst 1 $ \ [t] -> ([], list t))
        , ("::", inst 1 $ \ [t] -> ([t, list t], list t))
        ]
        ++ map tupleCtor [0..9]
  in
    List.foldl' makeCtorsHelp builtins unions


type VarDict =
  Map.Map Text Variable


makeCtorsHelp :: Dict Instantiator -> Module.CanonicalUnion -> Dict Instantiator
makeCtorsHelp constructorDict ( var, (rawTypeVars, ctors) ) =
  let
    inst :: Dict Instantiator -> (Var.Canonical, [T.Canonical]) -> Dict Instantiator
    inst ctorDict (name, rawArgs) =
      Map.insert
        (Var.toText name)
        ( do  ((args, tipe), dict) <- State.runStateT (go rawArgs) Map.empty
              return ( length args, Map.elems dict, args, tipe )
        )
        ctorDict

    tvars =
      map T.Var rawTypeVars

    go :: [T.Canonical] -> State.StateT VarDict IO ([Type], Type)
    go args =
      do  types <- mapM (instantiator nameToFlex Map.empty) args
          returnType <- instantiator nameToFlex Map.empty (T.Type var tvars)
          return (types, returnType)
  in
    List.foldl' inst constructorDict ctors



-- ACCESS TYPES


{-# INLINE get #-}
get :: Dict a -> Text -> a
get dict var =
  dict Map.! var


freshDataScheme :: Env -> Text -> Instantiator
freshDataScheme env var =
  get (_ctor env) var


ctorNames :: Env -> [Text]
ctorNames env =
  Map.keys (_ctor env)



-- UPDATE ENVIRONMENT


addValues :: VarDict -> Env -> Env
addValues newValues env =
  env { _value = Map.union (_value env) (Map.map VarN newValues) }



-- INSTANTIATE TYPES


instantiateFlex :: Env -> T.Canonical -> IO (Type, VarDict)
instantiateFlex env sourceType =
  State.runStateT (instantiator nameToFlex (_value env) sourceType) Map.empty


instantiateRigid :: Env -> T.Canonical -> IO (Type, VarDict)
instantiateRigid env sourceType =
  State.runStateT (instantiator nameToRigid (_value env) sourceType) Map.empty


instantiator :: (Text -> IO Variable) -> Dict Type -> T.Canonical -> State.StateT VarDict IO Type
instantiator nameToVar values sourceType =
    instantiatorHelp nameToVar values Set.empty sourceType


instantiatorHelp :: (Text -> IO Variable) -> Dict Type -> Set.Set Text -> T.Canonical -> State.StateT VarDict IO Type
instantiatorHelp nameToVar values aliasVars sourceType =
  {-# SCC elm_compiler_type_instantiate #-}
  let
    go =
      instantiatorHelp nameToVar values aliasVars
  in
    case sourceType of
      T.Lambda t1 t2 ->
          FunN <$> go t1 <*> go t2

      T.Var name ->
          if Set.member name aliasVars then
              return (PlaceHolder name)

          else
              case Map.lookup name values of
                Just tipe ->
                  return tipe

                Nothing ->
                  do  dict <- State.get
                      case Map.lookup name dict of
                        Just variable ->
                          return (VarN variable)

                        Nothing ->
                          do  variable <- State.liftIO (nameToVar name)
                              State.put (Map.insert name variable dict)
                              return (VarN variable)

      T.Aliased name args aliasType ->
          do  targs <- traverse (traverse go) args
              realType <-
                  case aliasType of
                    T.Filled tipe ->
                        instantiatorHelp nameToVar values Set.empty tipe

                    T.Holey tipe ->
                        instantiatorHelp nameToVar values (Set.fromList (map fst args)) tipe

              return (AliasN name targs realType)

      T.Type name args ->
          AppN name <$> traverse go args

      T.Record fields ext ->
          do  tfields <- traverse go (Map.fromList fields)
              text <-
                  case ext of
                    Nothing ->
                        return EmptyRecordN

                    Just extType ->
                        go extType

              return $ RecordN tfields text
