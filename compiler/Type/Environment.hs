module Type.Environment where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import qualified Control.Monad.State as State
import qualified Data.Traversable as Traverse
import qualified Data.Map as Map
import Data.Map ((!))
import Data.List (isPrefixOf)
import qualified Data.UnionFind.IO as UF

import qualified SourceSyntax.Type as Src
import SourceSyntax.Module (ADT)
import Type.Type

data Environment = Environment {
  constructor :: Map.Map String (IO (Int, [Variable], [Type], Type)),
  types :: Map.Map String Type,
  value :: Map.Map String Type
}

initialEnvironment :: [ADT] -> IO Environment
initialEnvironment datatypes = do
    types <- makeTypes datatypes

    return $ Environment {
      constructor = makeConstructors types datatypes,
      types = types,
      value = Map.empty
    }

makeTypes :: [ADT] -> IO (Map.Map String Type)
makeTypes datatypes = 
    Map.fromList <$> mapM makeCtor (builtins ++ map nameAndKind datatypes)
  where
    nameAndKind (name, tvars, _) = (name, length tvars)

    makeCtor (name, kind) = do
      ctor <- VarN <$> namedVar Constant name
      return (name, ctor)

    tuple n = ("_Tuple" ++ show n, n)

    kind n names = map (\name -> (name, n)) names

    builtins :: [(String,Int)]
    builtins = concat [ map tuple [0..9]
                      , kind 1 ["_List","Signal"]
                      , kind 0 ["Int","Float","Char","Bool","Element","Text"
                               ,"Color","Order"]
                      ]


makeConstructors :: Map.Map String Type
                 -> [ADT]
                 -> Map.Map String (IO (Int, [Variable], [Type], Type))
makeConstructors types datatypes = Map.fromList builtins
  where
    list  t = (types ! "_List") <| t

    inst :: Int -> ([Type] -> ([Type], Type)) -> IO (Int, [Variable], [Type], Type)
    inst numTVars tipe = do
      vars <- forM [1..numTVars] $ \_ -> var Flexible
      let (args, result) = tipe (map VarN vars)
      return (length args, vars, args, result)

    tupleCtor n =
        let name = "_Tuple" ++ show n
        in  (name, inst n $ \vs -> (vs, foldl (<|) (types ! name) vs))
    
    builtins :: [ (String, IO (Int, [Variable], [Type], Type)) ]
    builtins = [ ("[]"     , inst 1 $ \ [t] -> ([], list t))
               , ("::"     , inst 1 $ \ [t] -> ([t, list t], list t))
               ] ++ map tupleCtor [0..9]
                 ++ concatMap (ctorToType tempEnv) datatypes

    tempEnv = Environment { types = types, constructor = Map.empty, value = Map.empty }

ctorToType :: Environment -> ADT -> [ (String, IO (Int, [Variable], [Type], Type)) ]
ctorToType env (name, tvars, ctors) =
    zip (map fst ctors) (map inst ctors)
  where
    inst :: (String, [Src.Type]) -> IO (Int, [Variable], [Type], Type)
    inst ctor = do
      ((args, tipe), dict) <- State.runStateT (go ctor) Map.empty
      return (length args, Map.elems dict, args, tipe)
      

    go :: (String, [Src.Type]) -> State.StateT (Map.Map String Variable) IO ([Type], Type)
    go (ctor, args) = do
      types <- mapM (instantiator env) args
      returnType <- instantiator env (Src.Data name (map Src.Var tvars))
      return (types, returnType)


get :: Environment -> (Environment -> Map.Map String a) -> String -> a
get env subDict key = Map.findWithDefault err key (subDict env)
  where
    err = error $ "Could not find '" ++ key ++ "' in the type environment."


freshDataScheme :: Environment -> String -> IO (Int, [Variable], [Type], Type)
freshDataScheme env name = get env constructor name

instantiateType ::
    Environment -> Src.Type -> Map.Map String Variable -> IO ([Variable], Type)
instantiateType env sourceType dict =
  do (tipe, dict') <- State.runStateT (instantiator env sourceType) dict
     return (Map.elems dict', tipe)

instantiator :: Environment -> Src.Type -> State.StateT (Map.Map String Variable) IO Type
instantiator env sourceType = go sourceType
  where
    go :: Src.Type -> State.StateT (Map.Map String Variable) IO Type
    go sourceType =
      case sourceType of
        Src.Lambda t1 t2 -> (==>) <$> go t1 <*> go t2

        Src.Var x -> do
          dict <- State.get
          case Map.lookup x dict of
            Just var -> return (VarN var)
            Nothing ->
                do var <- State.liftIO $ namedVar flex x
                   State.put (Map.insert x var dict)
                   return (VarN var)
                where
                  flex | "number"     `isPrefixOf` x = Is Number
                       | "comparable" `isPrefixOf` x = Is Comparable
                       | "appendable" `isPrefixOf` x = Is Appendable
                       | otherwise = Flexible

        Src.Data "String" [] ->
            return (get env types "_List" <| get env types "Char")

        Src.Data name ts -> do
          ts' <- mapM go ts
          return $ foldl (<|) (get env types name) ts'

        Src.EmptyRecord -> return (TermN EmptyRecord1)

        Src.Record fields ext ->
          TermN <$> (Record1 <$> Traverse.traverse (mapM go) fields <*> go ext)
