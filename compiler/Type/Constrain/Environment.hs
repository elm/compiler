module Type.Constrain.Environment where

import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.UnionFind.IO as UF

import Type.Type

data Environment = Environment {
  constructor :: Map.Map String ([Variable], Type),
  builtin :: Map.Map String Type,
  value :: Map.Map String Type
}

initialEnvironment :: IO Environment
initialEnvironment = do
    let mkPair name = fmap ((,) name . VarN) (namedVar name)
    list <- mkPair "[]"
    prims <- mapM mkPair ["Int","Float","Char","Bool","Element"]
    
    cons <- do v <- flexibleVar
               let vlist = TermN (App1 (snd list) (VarN v))
               return ([v], VarN v ==> vlist ==> vlist)

    let builtins = list : prims
    return $ Environment {
      constructor = Map.singleton "::" cons,
      builtin = Map.fromList builtins,
      value = Map.empty
    }

get :: Environment -> (Environment -> Map.Map String a) -> String -> a
get env subDict key = subDict env ! key