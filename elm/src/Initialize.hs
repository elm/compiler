module Initialize (initialize) where

import Control.Monad.Error
import Data.List (lookup)

import Ast
import Parser (parseProgram)
import Hints
import Unify
import Rename
import Types ((-:))
import Optimize


initialize str = do
  (Module name ex im defs (ims,exs), tipes) <- parseProgram str
  let (Let ds _) = rename . Let defs $ Var "_"
  let dict n = maybe n id . lookup n $ zip (map fst defs) (map fst ds)
  let allHints = hints ++ tipes ++ ffiHints (ims,exs)
  subs <- unify allHints . Let ds $ checkFFI dict ims exs
  let Let defs' _ = optimize (Let ds $ Var "_")
  let im' = if any ((=="Prelude") . fst) im then im else
                ("Prelude", Importing []):im
  let exs' = map (\(js,n,t) -> (js,dict n,t)) exs
  return (subs `seq` Module name ex im' defs' (ims,exs'))


ffiHints (ims,exs) =
    map (\(_,_,n,t) -> n -: t) ims ++ map (\(_,n,t) -> n -: t) exs

checkFFI dict ims exs = list $ ims' ++ exs'
  where ims' = map (\(_,b,n,_)-> Binop "==" (Var n) (App (Var "constant") b)) ims
        exs' = map (\(_,n,_)  -> Binop "==" (Var n) (Var $ dict n)) exs