{-# OPTIONS_GHC -Wall #-}
module Type.Environment
    ( Env
    , initialize
    , getType, freshDataScheme, ctorNames
    , addValues
    , instantiateType
    )
    where

import qualified Control.Monad.State as State
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST.Type as T
import qualified AST.Variable as V
import qualified AST.Module as Module
import Type.Type


type TypeDict = Map.Map String Type
type VarDict = Map.Map String Variable


data Env = Env
    { _constructor :: Map.Map String (IO (Int, [Variable], [Type], Type))
    , _types :: TypeDict
    , _value :: TypeDict
    }


initialize :: [Module.CanonicalUnion] -> IO Env
initialize datatypes =
  do  types <- makeTypes datatypes
      let env =
            Env
              { _constructor = Map.empty
              , _value = Map.empty
              , _types = types
              }
      return $ env { _constructor = makeConstructors env datatypes }


makeTypes :: [Module.CanonicalUnion] -> IO TypeDict
makeTypes datatypes =
  do  unions <- mapM makeImported datatypes
      bs <- mapM makeBuiltin builtins
      return (Map.fromList (unions ++ bs))
  where
    makeImported :: (V.Canonical, Module.UnionInfo V.Canonical) -> IO (String, Type)
    makeImported (name, _) =
      do  tvar <- mkAtom name
          return (V.toString name, VarN tvar)

    makeBuiltin :: (String, Int) -> IO (String, Type)
    makeBuiltin (name, _) =
      do  name' <- mkAtom (V.builtin name)
          return (name, VarN name')

    builtins :: [(String, Int)]
    builtins =
        concat
          [ map tuple [0..9]
          , kind 1 ["List"]
          , kind 0 ["Int","Float","Char","String","Bool"]
          ]
      where
        tuple n = ("_Tuple" ++ show n, n)
        kind n names = map (\name -> (name, n)) names


makeConstructors
    :: Env
    -> [Module.CanonicalUnion]
    -> Map.Map String (IO (Int, [Variable], [Type], Type))
makeConstructors env datatypes =
    Map.fromList builtins
  where
    list t =
      (_types env ! "List") <| t

    inst :: Int -> ([Type] -> ([Type], Type)) -> IO (Int, [Variable], [Type], Type)
    inst numTVars tipe =
      do  vars <- mapM (\_ -> mkVar Nothing) [1..numTVars]
          let (args, result) = tipe (map (VarN) vars)
          return (length args, vars, args, result)

    tupleCtor n =
        let name = "_Tuple" ++ show n
        in  (name, inst n $ \vs -> (vs, foldl (<|) (_types env ! name) vs))

    builtins :: [ (String, IO (Int, [Variable], [Type], Type)) ]
    builtins =
        [ ("[]", inst 1 $ \ [t] -> ([], list t))
        , ("::", inst 1 $ \ [t] -> ([t, list t], list t))
        ] ++ map tupleCtor [0..9]
          ++ concatMap (ctorToType env) datatypes


ctorToType
    :: Env
    -> (V.Canonical, Module.UnionInfo V.Canonical)
    -> [(String, IO (Int, [Variable], [Type], Type))]
ctorToType env (name, (tvars, ctors)) =
    zip (map (V.toString . fst) ctors) (map inst ctors)
  where
    inst :: (V.Canonical, [T.Canonical]) -> IO (Int, [Variable], [Type], Type)
    inst ctor =
      do  ((args, tipe), dict) <- State.runStateT (go ctor) Map.empty
          return (length args, Map.elems dict, args, tipe)


    go :: (V.Canonical, [T.Canonical]) -> State.StateT VarDict IO ([Type], Type)
    go (_, args) =
      do  types <- mapM (instantiator Flex env) args
          returnType <- instantiator Flex env (T.App (T.Type name) (map T.Var tvars))
          return (types, returnType)



-- ACCESS TYPES


get :: (Env -> Map.Map String a) -> Env -> String -> a
get subDict env key =
    Map.findWithDefault (error msg) key (subDict env)
  where
    msg = "Could not find type constructor `" ++ key ++ "` while checking types."


getType :: Env -> String -> Type
getType =
  get _types


freshDataScheme :: Env -> String -> IO (Int, [Variable], [Type], Type)
freshDataScheme =
  get _constructor


ctorNames :: Env -> [String]
ctorNames env =
  Map.keys (_constructor env)



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


instantiatorHelp :: Flex -> Env -> Set.Set String -> T.Canonical -> State.StateT VarDict IO Type
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
          do  targs <- mapM (\(arg,tipe) -> (,) arg <$> go tipe) args
              realType <-
                  case aliasType of
                    T.Filled tipe ->
                        instantiatorHelp flex env Set.empty tipe

                    T.Holey tipe ->
                        instantiatorHelp flex env (Set.fromList (map fst args)) tipe

              return (AliasN name targs realType)

      T.Type name ->
          case Map.lookup (V.toString name) (_types env) of
            Just tipe ->
                return tipe

            Nothing ->
                error $
                  "Could not find type constructor `" ++
                  V.toString name ++ "` while checking types."

      T.App func args ->
          do  tfunc <- go func
              targs <- mapM go args
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
