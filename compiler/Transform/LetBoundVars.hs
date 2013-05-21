module LetBoundVars (letBoundVars) where

import Context
import Ast

class LetBoundVars a where
  letBoundVars :: a -> [String]

instance LetBoundVars a => LetBoundVars [a] where
  letBoundVars = concatMap letBoundVars

instance LetBoundVars Statement where
  letBoundVars stmt =
    case stmt of
      Definition d -> letBoundVars d
      Datatype _ _ tcs -> []
      ImportEvent _ e _ _ -> letBoundVars e
      ExportEvent _ _ _ -> []
      TypeAnnotation _ _ -> []
      TypeAlias _ _ _ -> []

instance LetBoundVars Def where
  letBoundVars (FnDef n _ e) = n : letBoundVars e
  letBoundVars (OpDef _ _ _ e) = letBoundVars e

instance LetBoundVars e => LetBoundVars (Context e) where
  letBoundVars (C _ _ e) = letBoundVars e

instance LetBoundVars Expr where
  letBoundVars expr =
    let f = letBoundVars in
    case expr of
      IntNum _ -> []
      FloatNum _ -> []
      Chr _ -> []
      Str _ -> []
      Boolean _ -> []
      Range e1 e2 -> f e1 ++ f e2
      Access e _ -> []
      Remove e _ -> []
      Insert e1 _ e2 -> f e1 ++ f e2
      Modify e ps -> f e ++ concatMap (f . snd) ps
      Record trps -> concatMap (\(_,_,e) -> f e) trps
      Binop op e1 e2 -> f e1 ++ f e2
      Lambda x e -> f e
      App e1 e2 -> f e1 ++ f e2
      If e1 e2 e3 -> concatMap f [e1,e2,e3]
      MultiIf ps -> concatMap (\(b,e) -> f b ++ f e) ps
      Let defs e -> concatMap letBoundVars defs ++ f e
      Var x -> []
      Data name es -> concatMap f es
      Case e cases -> f e ++ concatMap (f . snd) cases
      Markdown _ -> []
