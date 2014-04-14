{-# OPTIONS_GHC -Wall #-}
module Transform.Canonicalize.Environment where

import Control.Arrow (second)
import qualified Data.Map as Map

import AST.Expression.General (saveEnvName)
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var

type Dict a = Map.Map String [a]

dict :: [(String,a)] -> Dict a
dict pairs = Map.fromList $ map (second (:[])) pairs

insert :: String -> a -> Dict a -> Dict a
insert key value = Map.insertWith (++) key [value]

data Environment = Env
    { _values   :: Dict Var.Canonical
    , _adts     :: Dict Var.Canonical
    , _aliases  :: Dict (Var.Canonical, [String], Type.CanonicalType)
    , _patterns :: Dict Var.Canonical
    }

builtIns :: Environment
builtIns =
    Env { _values   = builtIn ([saveEnvName,"::","[]"] ++ tuples)
        , _adts     = builtIn ["_List","Int","Float","Char","Bool","String"]
        , _aliases  = dict []
        , _patterns = builtIn (["::","[]"] ++ tuples)
        }
    where
      builtIn xs = dict $ map (\x -> (x, Var.Canonical Var.BuiltIn x)) xs
      tuples = map (\n -> "_Tuple" ++ show (n :: Int)) [0..9]

update :: P.Pattern var -> Environment -> Environment
update ptrn env =
    env { _values = foldr put (_values env) (P.boundVarList ptrn) }
    where
      put x = Map.insert x [Var.local x]

merge :: Environment -> Environment -> Environment
merge (Env v1 t1 a1 p1) (Env v2 t2 a2 p2) =
    Env { _values   = Map.unionWith (++) v1 v2
        , _adts     = Map.unionWith (++) t1 t2
        , _aliases  = Map.unionWith (++) a1 a2
        , _patterns = Map.unionWith (++) p1 p2
        }
