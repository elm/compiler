module Initialize (initialize) where

import Control.Monad.Error

import Ast
import Parser (parseProgram)
import Hints
import Unify
import Rename
import Optimize

initialize str = do
  (Module name ex im defs jsffi, tipes) <- parseProgram str
  let expr = rename . Let defs $ Var "_"
  let allHints = liftM2 (\a b -> a ++ b ++ map ffiHint jsffi) hints tipes
  subs <- unify allHints expr
  let Let defs' _ = optimize expr
  let im' = if any ((=="Prelude") . fst) im then im else
                ("Prelude", Importing []):im
  return (subs `seq` Module name ex im' defs' jsffi)


ffiHint x = case x of
              ImportValue _ n t   -> (n,t)
              ExportValue _ n t   -> (n,t)
              ImportEvent _ n _ t -> (n,t)
              ExportEvent _ n t   -> (n,t)
