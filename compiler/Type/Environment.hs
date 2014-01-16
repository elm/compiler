{-# OPTIONS_GHC -W #-}
module Type.Environment where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (try, SomeException)
import Control.Monad
import Control.Monad.Trans.Error (ErrorList(..))
import Control.Monad.Error (ErrorT, throwError, liftIO)
import qualified Control.Monad.State as State
import qualified Data.Traversable as Traverse
import qualified Data.Map as Map
import Data.List (isPrefixOf)
import qualified Text.PrettyPrint as PP

import qualified SourceSyntax.Type as Src
import SourceSyntax.Module (ADT, Alias)
import Type.Type

type TypeDict = Map.Map String Type
type VarDict = Map.Map String Variable

data Environment = Environment {
  constructor :: Map.Map String (IO (Int, [Variable], [Type], Type)),
  aliases :: Map.Map String ([String], Src.Type),
  types :: TypeDict,
  value :: TypeDict
}

initialEnvironment :: [ADT] -> [Alias] -> IO Environment
initialEnvironment datatypes aliases = do
    types <- makeTypes datatypes
    let aliases' = Map.fromList $ map (\(a,b,c,_) -> (a,(b,c))) aliases
        env = Environment {
                constructor = Map.empty,
                value = Map.empty,
                types = types,
                aliases = aliases' }

    return $ env { constructor = makeConstructors env datatypes }

makeTypes :: [ADT] -> IO TypeDict
makeTypes datatypes = 
    Map.fromList <$> mapM makeCtor (builtins ++ map nameAndKind datatypes)
  where
    nameAndKind (name, tvars, _, _) = (name, length tvars)

    makeCtor (name, _) = do
      ctor <- VarN <$> namedVar Constant name
      return (name, ctor)

    tuple n = ("_Tuple" ++ show n, n)

    kind n names = map (\name -> (name, n)) names

    builtins :: [(String,Int)]
    builtins = concat [ map tuple [0..9]
                      , kind 1 ["_List"]
                      , kind 0 ["Int","Float","Char","String","Bool"]
                      ]


makeConstructors :: Environment
                 -> [ADT]
                 -> Map.Map String (IO (Int, [Variable], [Type], Type))
makeConstructors env datatypes = Map.fromList builtins
  where
    list t = (types env Map.! "_List") <| t

    inst :: Int -> ([Type] -> ([Type], Type)) -> IO (Int, [Variable], [Type], Type)
    inst numTVars tipe = do
      vars <- forM [1..numTVars] $ \_ -> var Flexible
      let (args, result) = tipe (map VarN vars)
      return (length args, vars, args, result)

    tupleCtor n =
        let name = "_Tuple" ++ show n
        in  (name, inst n $ \vs -> (vs, foldl (<|) (types env Map.! name) vs))
    
    builtins :: [ (String, IO (Int, [Variable], [Type], Type)) ]
    builtins = [ ("[]", inst 1 $ \ [t] -> ([], list t))
               , ("::", inst 1 $ \ [t] -> ([t, list t], list t))
               ] ++ map tupleCtor [0..9]
                 ++ concatMap (ctorToType env) datatypes


ctorToType :: Environment -> ADT -> [ (String, IO (Int, [Variable], [Type], Type)) ]
ctorToType env (name, tvars, ctors, _) =
    zip (map fst ctors) (map inst ctors)
  where
    inst :: (String, [Src.Type]) -> IO (Int, [Variable], [Type], Type)
    inst ctor = do
      ((args, tipe), (dict,_)) <- State.runStateT (go ctor) (Map.empty, Map.empty)
      return (length args, Map.elems dict, args, tipe)
      

    go :: (String, [Src.Type]) -> State.StateT (VarDict, TypeDict) IO ([Type], Type)
    go (_, args) = do
      types <- mapM (instantiator env) args
      returnType <- instantiator env (Src.Data name (map Src.Var tvars))
      return (types, returnType)


get :: Environment -> (Environment -> Map.Map String a) -> String -> a
get env subDict key = Map.findWithDefault err key (subDict env)
  where
    err = error $ "\nCould not find type constructor '" ++ key ++ "' while checking types."


freshDataScheme :: Environment -> String -> IO (Int, [Variable], [Type], Type)
freshDataScheme env name = get env constructor name

instance ErrorList PP.Doc where
  listMsg str = [PP.text str]

instantiateType :: Environment -> Src.Type -> VarDict -> ErrorT [PP.Doc] IO ([Variable], Type)
instantiateType env sourceType dict =
  do result <- liftIO $ try (State.runStateT (instantiator env sourceType) (dict, Map.empty))
     case result :: Either SomeException (Type, (VarDict, TypeDict)) of
       Left someError -> throwError [ PP.text $ show someError ]
       Right (tipe, (dict',_)) -> return (Map.elems dict', tipe)

instantiator :: Environment -> Src.Type
             -> State.StateT (VarDict, TypeDict) IO Type
instantiator env sourceType = go sourceType
  where
    go :: Src.Type -> State.StateT (VarDict, TypeDict) IO Type
    go sourceType =
      case sourceType of
        Src.Lambda t1 t2 -> (==>) <$> go t1 <*> go t2

        Src.Var x -> do
          (dict, aliases) <- State.get
          case (Map.lookup x dict, Map.lookup x aliases) of
            (_, Just t) -> return t
            (Just v, _) -> return (VarN v)
            _ ->
                do var <- State.liftIO $ namedVar flex x
                   State.put (Map.insert x var dict, aliases)
                   return (VarN var)
                where
                  flex | "number"     `isPrefixOf` x = Is Number
                       | "comparable" `isPrefixOf` x = Is Comparable
                       | "appendable" `isPrefixOf` x = Is Appendable
                       | otherwise = Flexible

        Src.Data name ts -> do
          ts' <- mapM go ts
          case (Map.lookup name (types env), Map.lookup name (aliases env)) of
            (Just t, _) -> return $ foldl (<|) t ts'
            (_, Just (tvars, t)) ->
                let tvarLen = length tvars
                    msg = "\nType alias '" ++ name ++ "' expects " ++ show tvarLen ++
                          " type argument" ++ (if tvarLen == 1 then "" else "s") ++
                          " but was given " ++ show (length ts')
                in  if length ts' /= length tvars then error msg else
                        do (dict, aliases) <- State.get
                           let aliases' = Map.union (Map.fromList $ zip tvars ts') aliases
                           State.put (dict, aliases')
                           t' <- go t
                           State.put (dict, aliases)
                           return t'
            _ -> error $ "\nCould not find type constructor '" ++
                         name ++ "' while checking types."

        Src.Record fields ext -> do
          fields' <- Traverse.traverse (mapM go) (Src.fieldMap fields)
          ext' <- case ext of
                    Nothing -> return $ TermN EmptyRecord1
                    Just x -> go (Src.Var x)
          return $ TermN (Record1 fields' ext')