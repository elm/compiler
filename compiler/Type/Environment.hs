module Type.Environment where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import qualified Control.Monad.State as State
import qualified Data.Traversable as Traverse
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.UnionFind.IO as UF

import qualified SourceSyntax.Type as Src
import Type.Type

data Environment = Environment {
  constructor :: Map.Map String (IO (Int, [Variable], Type)),
  types :: Map.Map String Type,
  value :: Map.Map String Type
}

initialEnvironment :: [(String, [String], [(String,[Src.Type])])] -> IO Environment
initialEnvironment datatypes = do
    types <- makeTypes datatypes

    return $ Environment {
      constructor = makeConstructors types datatypes,
      types = types,
      value = Map.empty
    }

makeTypes datatypes = 
    Map.fromList <$> mapM makeCtor (builtins ++ map nameAndKind datatypes)
  where
    nameAndKind (name, tvars, _) = (name, length tvars)

    makeCtor (name, kind) = do
      ctor <- VarN <$> namedVar name
      return (name, ctor)

    tuple n = ("_Tuple" ++ show n, n)

    kind n names = map (\name -> (name, n)) names

    builtins :: [(String,Int)]
    builtins = concat [ map tuple [0..10]
                      , kind 1 ["_List","Maybe","Signal"]
                      , kind 0 ["Int","Float","Char","Bool","Element"]
                      ]

makeConstructors types datatypes =
    Map.union (Map.fromList builtins) (Map.unions $ map (instances dummyEnv) datatypes)
  where
    dummyEnv = Environment { constructor=Map.empty, types=types, value=Map.empty }

    list  t = (types ! "_List") <| t
    maybe t = (types ! "Maybe") <| t

    inst :: Int -> Int -> ([Type] -> Type) -> IO (Int, [Variable], Type)
    inst kind numTVars tipe = do
       vars <- forM [1..numTVars] $ \_ -> flexibleVar
       return (kind, vars, tipe $ map VarN vars)

    tupleCtor n =
        let name = "_Tuple" ++ show n
        in  (name, inst n n $ \vs -> foldr (==>) (foldl (<|) (types ! name) vs) vs)
    
    builtins :: [ (String, IO (Int, [Variable], Type)) ]
    builtins = [ ("Nothing", inst 0 1 $ \ [t] -> maybe t)
               , ("Just"   , inst 1 1 $ \ [t] -> t ==> maybe t)
               , ("[]"     , inst 0 1 $ \ [t] -> list t)
               , ("::"     , inst 2 1 $ \ [t] -> t ==> list t ==> list t)
               ] ++ map tupleCtor [0..10]

   

get :: Environment -> (Environment -> Map.Map String a) -> String -> a
get env subDict key = Map.findWithDefault err key (subDict env)
  where
    err = error $ "Could not find '" ++ key ++ "' in the type environment."


freshDataScheme :: Environment -> String -> IO (Int, [Variable], Type)
freshDataScheme env name = do
  putStr "fresh " >> putStrLn name
  get env constructor name


instances :: Environment
          -> (String, [String], [(String, [Src.Type])])
          -> Map.Map String (IO (Int, [Variable], Type))
instances env (name, tvars, ctors) = Map.fromList $ map go ctors
  where
    tipe = Src.Data name (map Src.Var tvars)

    go (ctor, tipes) = (,) ctor $ do
      let funcType = foldr Src.Lambda tipe tipes
      (vars, ttype) <- instantiateTypeWithContext env funcType Map.empty
      return (length tipes, vars, ttype)

instantiateType :: Environment -> Src.Type -> IO Type
instantiateType env sourceType =
    snd <$> instantiateTypeWithContext env sourceType Map.empty

instantiateTypeWithContext :: Environment
                           -> Src.Type
                           -> Map.Map String Variable
                           -> IO ([Variable], Type)
instantiateTypeWithContext env sourceType dict =
  do (tipe, dict') <- State.runStateT (go sourceType) dict
     return (Map.elems dict', tipe)
  where
    go :: Src.Type -> State.StateT (Map.Map String Variable) IO Type
    go sourceType =
      case sourceType of
        Src.Lambda t1 t2 -> TermN <$> (Fun1 <$> go t1 <*> go t2)

        Src.Var x -> do
          dict <- State.get
          case Map.lookup x dict of
            Just var -> return (VarN var)
            Nothing -> do
              var <- State.liftIO $ namedVar x -- should this be Constant or Flexible?
              State.put (Map.insert x var dict)
              return (VarN var)

        Src.Data name ts -> do
          State.liftIO $ putStr "instantiate " >> putStrLn name
          ts' <- mapM go ts
          return $ foldr (\t result -> TermN $ App1 t result) (get env types name) ts'

        Src.EmptyRecord -> return (TermN EmptyRecord1)

        Src.Record fields ext ->
          TermN <$> (Record1 <$> Traverse.traverse (mapM go) fields <*> go ext)
