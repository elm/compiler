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

import AST.Module (ADTs)
import qualified AST.Type as T
import qualified AST.Variable as V
import Type.Type

type TypeDict = Map.Map String Type
type VarDict = Map.Map String Variable

data Environment = Environment {
  constructor :: Map.Map String (IO (Int, [Variable], [Type], Type)),
  types :: TypeDict,
  value :: TypeDict
}

initialEnvironment :: ADTs -> IO Environment
initialEnvironment datatypes = do
    types <- makeTypes datatypes
    let env = Environment
              { constructor = Map.empty
              , value = Map.empty
              , types = types
              }
    return $ env { constructor = makeConstructors env datatypes }

makeTypes :: ADTs -> IO TypeDict
makeTypes datatypes =
  do adts <- Traverse.sequence (Map.mapWithKey make datatypes)
     bs   <- Map.fromList . zip (map fst builtins) <$> mapM (uncurry make) builtins
     return (Map.union adts bs)
  where
    make name _ = VarN <$> namedVar Constant name

    tuple n = ("_Tuple" ++ show n, n)

    kind n names = map (\name -> (name, n)) names

    builtins :: [(String,Int)]
    builtins = concat [ map tuple [0..9]
                      , kind 1 ["_List"]
                      , kind 0 ["Int","Float","Char","String","Bool"]
                      ]


makeConstructors :: Environment
                 -> ADTs
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
                 ++ concatMap (ctorToType env) (Map.toList datatypes)


--ctorToType :: Environment -> ADT -> [ (String, IO (Int, [Variable], [Type], Type)) ]
ctorToType env (name, (tvars, ctors)) =
    zip (map fst ctors) (map inst ctors)
  where
    inst :: (String, [T.CanonicalType]) -> IO (Int, [Variable], [Type], Type)
    inst ctor = do
      ((args, tipe), dict) <- State.runStateT (go ctor) Map.empty
      return (length args, Map.elems dict, args, tipe)
      

    go :: (String, [T.CanonicalType]) -> State.StateT VarDict IO ([Type], Type)
    go (_, args) = do
      types <- mapM (instantiator env) args
      returnType <- instantiator env (T.Data (error "name") (map T.Var tvars))
      return (types, returnType)


get :: Environment -> (Environment -> Map.Map String a) -> String -> a
get env subDict key = Map.findWithDefault err key (subDict env)
  where
    err = error $ "\nCould not find type constructor '" ++ key ++ "' while checking types."


freshDataScheme :: Environment -> String -> IO (Int, [Variable], [Type], Type)
freshDataScheme env name = get env constructor name

instance ErrorList PP.Doc where
  listMsg str = [PP.text str]

instantiateType :: Environment -> T.CanonicalType -> VarDict -> ErrorT [PP.Doc] IO ([Variable], Type)
instantiateType env sourceType dict =
  do result <- liftIO $ try (State.runStateT (instantiator env sourceType) dict)
     case result :: Either SomeException (Type, VarDict) of
       Left someError -> throwError [ PP.text $ show someError ]
       Right (tipe, dict') -> return (Map.elems dict', tipe)

instantiator :: Environment -> T.CanonicalType -> State.StateT VarDict IO Type
instantiator env sourceType = go sourceType
  where
    go :: T.CanonicalType -> State.StateT VarDict IO Type
    go sourceType =
      case sourceType of
        T.Lambda t1 t2 -> (==>) <$> go t1 <*> go t2

        T.Var x -> do
          dict <- State.get
          case Map.lookup x dict of
            Just v -> return (VarN v)
            Nothing ->
                do var <- State.liftIO $ namedVar flex x
                   State.put (Map.insert x var dict)
                   return (VarN var)
                where
                  flex | "number"     `isPrefixOf` x = Is Number
                       | "comparable" `isPrefixOf` x = Is Comparable
                       | "appendable" `isPrefixOf` x = Is Appendable
                       | otherwise = Flexible

        T.Data name ts -> do
          ts' <- mapM go ts
          case Map.lookup (V.toString name) (types env) of
            Just t -> return $ foldl (<|) t ts'
            Nothing -> error $ "\nCould not find type constructor '" ++
                               V.toString name ++ "' while checking types."

        T.Record fields ext -> do
          fields' <- Traverse.traverse (mapM go) (T.fieldMap fields)
          ext' <- case ext of
                    Nothing -> return $ TermN EmptyRecord1
                    Just x -> go x
          return $ TermN (Record1 fields' ext')
