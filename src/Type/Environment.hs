{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Environment
    ( Env
    , Dict
    , initialize
    , get, getType, freshDataScheme, ctorNames
    , addValues
    , instantiateType
    )
    where

import Control.Monad (foldM)
import qualified Control.Monad.State as State
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
    , _types :: Dict Type
    , _value :: Dict Type
    }


type Instantiator =
  IO (Int, [Variable], [Type], Type)


type Dict a =
  Map.Map Text a


initialize :: [Module.CanonicalUnion] -> IO Env
initialize unions =
  do  types <- makeTypes unions
      let env =
            Env
              { _ctor = Map.empty
              , _value = Map.empty
              , _types = types
              }
      return $ env { _ctor = makeCtors env unions }



-- TYPES


makeTypes :: [Module.CanonicalUnion] -> IO (Dict Type)
makeTypes unions =
  do  builtinDict <- Map.fromList <$> mapM makeBuiltin builtinTypes
      foldM addImported builtinDict unions


addImported :: Dict Type -> (Var.Canonical, unionInfo) -> IO (Dict Type)
addImported dict ( var, _ ) =
  do  atom <- mkAtom var
      return (Map.insert (Var.toText var) (VarN atom) dict)


makeBuiltin :: (Text, Int) -> IO (Text, Type)
makeBuiltin (name, _) =
  do  atom <- mkAtom (Var.builtin name)
      return (name, VarN atom)


builtinTypes :: [(Text, Int)]
builtinTypes =
    concat
      [ map tuple [0..9]
      , kind 1 ["List"]
      , kind 0 ["Int","Float","Char","String","Bool"]
      ]
  where
    tuple size = ( Help.makeTuple size, size )
    kind n names = map (\name -> (name, n)) names



-- CONSTRUCTORS


makeCtors :: Env -> [Module.CanonicalUnion] -> Dict Instantiator
makeCtors env unions =
  let
    list t =
      getType env "List" <| t

    inst :: Int -> ([Type] -> ([Type], Type)) -> Instantiator
    inst numTVars tipe =
      do  vars <- mapM (\_ -> mkVar Nothing) [1..numTVars]
          let (args, result) = tipe (map (VarN) vars)
          return (length args, vars, args, result)

    tupleCtor size =
      let
        name =
          Help.makeTuple size
      in
        (name, inst size $ \vs -> (vs, foldl (<|) (getType env name) vs))

    builtins :: Dict Instantiator
    builtins =
      Map.fromList $
        [ ("[]", inst 1 $ \ [t] -> ([], list t))
        , ("::", inst 1 $ \ [t] -> ([t, list t], list t))
        ]
        ++ map tupleCtor [0..9]
  in
    List.foldl' (makeCtorsHelp env) builtins unions


type VarDict =
  Map.Map Text Variable


makeCtorsHelp :: Env -> Dict Instantiator -> Module.CanonicalUnion -> Dict Instantiator
makeCtorsHelp env constructorDict ( var, (rawTypeVars, ctors) ) =
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
      do  types <- mapM (instantiator Flex env) args
          returnType <- instantiator Flex env (T.App (T.Type var) tvars)
          return (types, returnType)
  in
    List.foldl' inst constructorDict ctors



-- ACCESS TYPES


{-# INLINE get #-}
get :: Dict a -> Text -> a
get dict var =
  dict Map.! var


getType :: Env -> Text -> Type
getType env var =
  get (_types env) var


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


instantiateType :: Flex -> Env -> T.Canonical -> IO (Type, VarDict)
instantiateType flex env sourceType =
  State.runStateT (instantiator flex env sourceType) Map.empty


instantiator :: Flex -> Env -> T.Canonical -> State.StateT VarDict IO Type
instantiator flex env sourceType =
    instantiatorHelp flex env Set.empty sourceType


instantiatorHelp :: Flex -> Env -> Set.Set Text -> T.Canonical -> State.StateT VarDict IO Type
instantiatorHelp flex env aliasVars sourceType =
  let
    go =
      instantiatorHelp flex env aliasVars
  in
    case sourceType of
      T.Lambda t1 t2 ->
          (==>) <$> go t1 <*> go t2

      T.Var name ->
          if Set.member name aliasVars then
              return (PlaceHolder name)

          else
              do  dict <- State.get
                  case Map.lookup name (_value env) of
                    Just tipe ->
                      return tipe

                    Nothing ->
                      case Map.lookup name dict of
                        Just variable ->
                          return (VarN variable)

                        Nothing ->
                          do  variable <- State.liftIO (mkNamedVar flex name)
                              State.put (Map.insert name variable dict)
                              return (VarN variable)

      T.Aliased name args aliasType ->
          do  targs <- traverse (traverse go) args
              realType <-
                  case aliasType of
                    T.Filled tipe ->
                        instantiatorHelp flex env Set.empty tipe

                    T.Holey tipe ->
                        instantiatorHelp flex env (Set.fromList (map fst args)) tipe

              return (AliasN name targs realType)

      T.Type name ->
          return (getType env (Var.toText name))

      T.App func args ->
          do  tfunc <- go func
              targs <- traverse go args
              return $ foldl (<|) tfunc targs

      T.Record fields ext ->
          do  tfields <- traverse go (Map.fromList fields)
              text <-
                  case ext of
                    Nothing ->
                        return $ TermN EmptyRecord1

                    Just extType ->
                        go extType

              return $ TermN (Record1 tfields text)
