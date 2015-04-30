{-# OPTIONS_GHC -Wall #-}
module Transform.Canonicalize.Environment where

import Control.Arrow (second)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans (lift)
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set

import AST.Expression.General (saveEnvName)
import qualified AST.Module as Module
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var

import Text.PrettyPrint (Doc)


-- ENVIRONMENT

data Environment = Env
    { _home     :: Module.Name
    , _values   :: Dict Var.Canonical
    , _adts     :: Dict Var.Canonical
    , _aliases  :: Dict (Var.Canonical, [String], Type.CanonicalType)
    , _patterns :: Dict Var.Canonical
    }


builtIns :: Module.Name -> Environment
builtIns home =
    Env { _home     = home
        , _values   = builtIn (tuples ++ [saveEnvName])
        , _adts     = builtIn (tuples ++ ["List","Int","Float","Char","Bool","String"])
        , _aliases  = dict []
        , _patterns = builtIn (tuples ++ ["::","[]"])
        }
    where
      builtIn xs =
          dict $ map (\x -> (x, Var.Canonical Var.BuiltIn x)) xs

      tuples =
          map (\n -> "_Tuple" ++ show n) [0 .. 9 :: Int]


addPattern :: P.Pattern var -> Environment -> Environment
addPattern ptrn env =
    env { _values = foldr put (_values env) (P.boundVarList ptrn) }
  where
    put x =
      Map.insert x (Set.singleton (Var.local x))


merge :: Environment -> Environment -> Environment
merge (Env n1 v1 t1 a1 p1) (Env n2 v2 t2 a2 p2)
  | n1 /= n2 = error "trying to merge incompatable environments"
  | otherwise =
      Env { _home     = n1
          , _values   = Map.unionWith Set.union v1 v2
          , _adts     = Map.unionWith Set.union t1 t2
          , _aliases  = Map.unionWith Set.union a1 a2
          , _patterns = Map.unionWith Set.union p1 p2
          }


-- RAW NAMES to CANONICAL NAMES

type Dict a =
    Map.Map String (Set.Set a)


dict :: [(String,a)] -> Dict a
dict pairs =
  Map.fromList (map (second Set.singleton) pairs)


insert :: (Ord a) => String -> a -> Dict a -> Dict a
insert key value =
  Map.insertWith Set.union key (Set.singleton value)


-- CANONICALIZATION MANAGER

type Canonicalizer err a =
    ExceptT err (State.State (Set.Set Module.Name)) a


uses :: Module.Name -> Canonicalizer e ()
uses home =
  lift (State.modify (Set.insert home))


using :: Var.Canonical -> Canonicalizer String Var.Canonical
using var@(Var.Canonical home _) =
  do  case home of
        Var.BuiltIn     -> return ()
        Var.Module path -> uses path
        Var.Local       -> return ()
      return var


onError :: (String -> Doc) -> Canonicalizer String a -> Canonicalizer [Doc] a
onError handler canonicalizer =
  do  usedModules <- lift State.get
      let (result, usedModules') =
            State.runState (runExceptT canonicalizer) usedModules
      lift (State.put usedModules')
      case result of
        Left err -> throwError [handler err]
        Right x  -> return x
