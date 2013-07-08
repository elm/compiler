module Type.Environment where

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
    list <- mkPair "[_]"
    int <- mkPair "Int"
    prims <- mapM mkPair ["Float","Char","Bool","Element"]
    let builtins = list : int : prims
    
    cons <- do v <- flexibleVar
               let vlist = TermN (App1 (snd list) (VarN v))
               return ([v], VarN v ==> vlist ==> vlist)

    nil <- do v <- flexibleVar
              return ([v], TermN (App1 (snd list) (VarN v)))

    let add = snd int ==> snd int ==> snd int

    return $ Environment {
      constructor = Map.fromList [("::", cons), ("[]", nil)],
      builtin = Map.fromList builtins,
      value = Map.empty -- Map.fromList [("+", add)]
    }

get :: Environment -> (Environment -> Map.Map String a) -> String -> a
get env subDict key = Map.findWithDefault err key (subDict env)
  where
    err = error $ "Could not find '" ++ key ++ "' in the type environment."