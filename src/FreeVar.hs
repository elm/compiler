module FreeVar (freeIn) where

import Ast
import Data.Set hiding (map)

x `freeIn` expr = member x $ freeVars expr

freeVars expr =
    let f = freeVars in
    case expr of
      Range e1 e2 -> union (f e1) (f e2)
      Binop op e1 e2 -> union (f e1) (f e2)
      Lambda x e -> delete x (f e)
      App e1 e2 -> union (f e1) (f e2)
      If e1 e2 e3 -> unions $ map f [e1,e2,e3]
      Lift e es -> unions $ map f es
      Fold e1 e2 e3 -> unions $ map f [e1,e2,e3]
      Async e -> f e
      Let defs e -> foldr delete (unions (f e : map f es)) vs
              where (vs,es) = unzip defs
      Var x -> singleton x
      Data name es -> unions (map f es)
      Case e cases -> union (f e) (unions $ map caseFreeVars cases)
      _ -> empty

caseFreeVars (p,e) = difference (freeVars e) (pvars p)
    where pvars p = case p of
                      PAnything -> empty
                      PVar x -> singleton x
                      PData _ ps -> unions (map pvars ps)